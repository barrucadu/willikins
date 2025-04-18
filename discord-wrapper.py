import asyncio
import discord
import logging
import json
import os


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
write_config()

intents = discord.Intents.default()
intents.message_content = True
client = discord.Client(intents=intents)
tree = discord.app_commands.CommandTree(client)


async def willikins_respond(chat_id, query):
    proc = await asyncio.create_subprocess_exec(
        "willikins",
        "respond",
        chat_id,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )
    stdout, stderr = await proc.communicate(query.encode())
    return (proc.returncode, stdout.decode(), stderr.decode())


async def send_debug_message(client, guild_id, message):
    channel_id = CONFIG["debug-channels"].get(str(guild_id))
    if channel_id:
        channel = client.get_partial_messageable(channel_id)
        if channel:
            await channel.send(message)


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


@client.event
async def on_message(message):
    if message.author == client.user:
        return

    chat_id = f"DISCORD-{message.channel.id}"
    retcode, stdout, stderr = await willikins_respond(chat_id, message.content)
    if retcode == 0:
        reply = None
        for bot_message in json.loads(stdout):
            for bot_message_content in bot_message["content"]:
                ty = bot_message_content["type"]
                if ty == "tool_use":
                    await send_debug_message(
                        client,
                        message.guild.id,
                        f"[tool use (id {bot_message_content['id']}) (name {bot_message_content['name']})]\n{bot_message_content['input']}",
                    )
                elif ty == "tool_result":
                    await send_debug_message(
                        client,
                        message.guild.id,
                        f"[tool result (id {bot_message_content['tool_use_id']}) (status {'error' if bot_message_content['is_error'] else 'ok'})]\n{bot_message_content['content']}",
                    )
                elif ty == "text":
                    # sometimes if asked to use a tool, willikins will say he'll
                    # do it, then do it, then say it's been done - only forward
                    # the final message along to the channel
                    reply = bot_message_content["text"]
        if reply:
            await message.reply(reply)
    else:
        logging.warning(f"willikins error code {retcode}: {stderr}")
        await message.reply("My apologies, sir, I didn't quite catch that.")


@client.event
async def on_ready():
    await tree.sync()


client.run(os.getenv("DISCORD_BOT_TOKEN"))
