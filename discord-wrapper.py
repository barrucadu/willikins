import asyncio
import datetime
import discord
import discord.ext.tasks
import json
import logging
import os
import zoneinfo


TIMEZONE = zoneinfo.ZoneInfo("Europe/London")

DAILY_BRIEFING_TIME = datetime.time(
    hour=8,
    minute=30,
    tzinfo=TIMEZONE,
)

CONFIG_FILE_PATH = os.getenv("DISCORD_CONFIG_FILE", "willikins-discord.json")

try:
    with open(CONFIG_FILE_PATH, "r") as f:
        CONFIG = json.load(f)
except FileNotFoundError:
    CONFIG = {}


def write_config():
    with open(CONFIG_FILE_PATH, "w") as f:
        json.dump(CONFIG, f, indent=2, sort_keys=True)


CONFIG.setdefault("debug-channels", {})
CONFIG.setdefault("daily-briefings", {})
write_config()

intents = discord.Intents.default()
intents.message_content = True
client = discord.Client(intents=intents)
tree = discord.app_commands.CommandTree(client)


###############################################################################
## llm


async def willikins(command, chat_id=None, query=None):
    args = ["willikins", command]
    if chat_id is not None:
        args.append(f"--chat-id={chat_id}")

    proc = await asyncio.create_subprocess_exec(
        *args,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )
    stdout, stderr = await proc.communicate(None if query is None else query.encode())
    stdout = stdout.decode()
    stderr = stderr.decode()
    retcode = proc.returncode

    if retcode != 0:
        logging.warning(f"willikins error code {retcode}: {stderr}")

    return (retcode, stdout, stderr)


async def send_debug_message(guild_id, message):
    channel_id = CONFIG["debug-channels"].get(str(guild_id))
    if channel_id:
        channel = client.get_partial_messageable(channel_id)
        if channel:
            await channel.send(message)


async def send_willikins_response(guild_id, bot_messages, send_reply):
    reply = None
    for bot_message in bot_messages:
        for bot_message_content in bot_message["content"]:
            ty = bot_message_content["type"]
            if ty == "tool_use":
                await send_debug_message(
                    guild_id,
                    f"[tool use (id {bot_message_content['id']}) (name {bot_message_content['name']})]\n{bot_message_content['input']}",
                )
            elif ty == "tool_result":
                await send_debug_message(
                    guild_id,
                    f"[tool result (id {bot_message_content['tool_use_id']}) (status {'error' if bot_message_content['is_error'] else 'ok'})]\n{bot_message_content['content']}",
                )
            elif ty == "text":
                # sometimes if asked to use a tool, willikins will say he'll do
                # it, then do it, then say it's been done - only forward the
                # final message along to the channel
                reply = bot_message_content["text"]
    if reply:
        await send_reply(reply)


@tree.command(
    name="setdebug",
    description="Set this channel as the one where debug messages go",
)
async def set_debug_channel(interaction):
    guild_id = interaction.guild.id
    channel_id = interaction.channel_id
    logging.info(f"set debug channel for {guild_id} to {channel_id}")

    CONFIG["debug-channels"][str(guild_id)] = str(channel_id)
    write_config()

    await interaction.response.send_message("Certainly, sir.")


###############################################################################
## daily briefing


def should_send_daily_briefing(guild_id):
    last_sent_at = CONFIG["daily-briefings"][guild_id].get("last_sent_at_date")
    sent_today = last_sent_at == datetime.date.today().isoformat()
    due = datetime.datetime.now(tz=TIMEZONE).time() >= DAILY_BRIEFING_TIME
    return not sent_today and due


def mark_daily_briefing_as_sent(guild_id):
    if guild_id in CONFIG["daily-briefings"]:
        CONFIG["daily-briefings"][guild_id][
            "last_sent_at_date"
        ] = datetime.date.today().isoformat()
        write_config()


async def send_daily_briefing(guild_id, channel_id, send_message):
    chat_id = f"DISCORD-{channel_id}"
    retcode, stdout, stderr = await willikins("daily-briefing", chat_id)
    if retcode == 0:
        await send_willikins_response(guild_id, json.loads(stdout), send_message)
        mark_daily_briefing_as_sent(guild_id)
    else:
        await send_message(
            "My most humble apologies sir, I am unable to present my usual briefing."
        )


@tree.command(
    name="setdailybriefing",
    description="Set this channel as the one where the daily briefing goes",
)
async def set_daily_briefing_channel(interaction):
    guild_id = interaction.guild.id
    channel_id = interaction.channel_id
    logging.info(f"set daily briefing channel for {guild_id} to {channel_id}")

    CONFIG["daily-briefings"].setdefault(str(guild_id), {})
    CONFIG["daily-briefings"][str(guild_id)]["channel"] = str(channel_id)
    write_config()

    await interaction.response.send_message("Certainly, sir.")


@tree.command(
    name="dailybriefing",
    description="Generate the daily briefing",
)
async def daily_briefing(interaction):
    # Initial response has to be sent within 3s
    # https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-callback
    await interaction.response.defer(thinking=True)

    await send_daily_briefing(
        str(interaction.guild.id),
        str(interaction.channel_id),
        interaction.followup.send,
    )


# every *:[00, 15, 45], check if we've sent today's daily briefing, and send it
# if not.
EVERY_15TH_MINUTE = [
    datetime.time(hour=hour, minute=minute)
    for hour in range(0, 24)
    for minute in [0, 15, 30, 45]
]


@discord.ext.tasks.loop(time=EVERY_15TH_MINUTE)
async def task_daily_briefing():
    for guild_id, state in CONFIG["daily-briefings"].items():
        if should_send_daily_briefing(guild_id):
            await send_daily_briefing(
                guild_id,
                state["channel"],
                client.get_partial_messageable(state["channel"]).send,
            )


###############################################################################
## feeds


@tree.command(
    name="randomfeedentry",
    description="Get a random unread feed entry, if there is one",
)
async def random_feed_entry(interaction):
    await interaction.response.defer(thinking=True)

    chat_id = f"DISCORD-{interaction.channel_id}"
    retcode, stdout, stderr = await willikins("random-feed-entry")
    if retcode == 0:
        if stdout == "":
            stdout = "You have read all of your articles, sir."
        await interaction.followup.send(stdout)
    else:
        await interaction.followup.send(
            "My most humble apologies sir, I am unable to consult your feeds at the moment."
        )


###############################################################################
## events


@client.event
async def on_message(message):
    if message.author == client.user:
        return

    chat_id = f"DISCORD-{message.channel.id}"
    retcode, stdout, stderr = await willikins("respond", chat_id, message.content)
    if retcode == 0:
        await send_willikins_response(
            message.guild.id, json.loads(stdout), message.reply
        )
    else:
        await message.reply("My apologies, sir, I didn't quite catch that.")


@client.event
async def on_ready():
    task_daily_briefing.start()
    await tree.sync()


client.run(os.getenv("DISCORD_BOT_TOKEN"))
