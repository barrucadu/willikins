Willikins
=========

[insert cool AI generated butler image here]

> **User:** Give a short self-introduction targeted at someone who doesn't know
> you or your capabilities.
>
> **Willikins:** Greetings, I am Willikins, your digital butler.  I maintain
> your schedule and provide timely reminders for appointments and commitments.
> I can note down important information for later reference and answer questions
> about your calendar.  While I cannot perform physical tasks or access external
> websites, I excel at keeping your digital affairs in order with proper
> discretion and efficiency.  How might I assist you today?

Usage
-----

Set the following environment variables:

``` bash
ANTHROPIC_API_KEY="api-key"
GOOGLE_CALENDAR_ID="calendar-id"
GOOGLE_CREDENTIALS_FILE="/path/to/file"
DISCORD_BOT_TOKEN="token"
```

Ensure `gcloud` is in your `$PATH`.

Interact with Willikins in chatbot mode directly:

``` bash
nix run
```

Or run the Discord bot:

``` bash
python3 discord-wrapper.py
```

### Google Calendar integration

First create a service account:

1. Log into the Google Cloud console
2. [Create a new project](https://console.cloud.google.com/projectcreate) (or re-use an existing one)
3. [Enable the Google Calendar API](https://console.cloud.google.com/marketplace/product/google/calendar-json.googleapis.com)
4. [Create a service account](https://console.cloud.google.com/iam-admin/serviceaccounts)
5. Create a key for that service account, and download the key file (this is the path you set in `GOOGLE_CREDENTIALS_FILE`)

Then share your calendar with it:

1. Log into Google Calendar
2. Go to the "Settings and Sharing" page
3. Share your calendar with the service account's email address
4. Scroll down to the "Integrate calendar" section and note the "Calendar ID" (this is the value you set in `GOOGLE_CALENDAR_ID`)

### Discord bot set-up

Create a Discord bot account:

1. Log into the Discord website
2. [Create a new application](https://discord.com/developers/applications)
3. Navigate to the "Installation" section:
   1. Set "Install Link" to "None"
3. Navigate to the "Bot" section:
   1. Uncheck "Public Bot"
   2. Check "Message Content Intent"
   3. Note the "Token" (this is the value you set in `DISCORD_BOT_TOKEN`)
4. Navigate to the "Oauth2" section:
   1. Generate a URL with "applications.commands" and "bot" scopes and the following bot permissions:
      - View Channels
      - Send Messages
      - Send Messages in Threads
      - Embed Links
    2. Open the generated URL in your browser to invite the bot to your server
