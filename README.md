# gh-projects-discord

Relays `GitHub Projects` webhooks to `Discord`. Written in Haskell to have fun.

## Existing GitHub-Discord integration

Using the `/discord` endpoint, most common webhooks like "issue created" or "commit pushed" are shows as Discord embedes in the directed channel. However, webhooks on GitHub Projects are entirely ignored. This is possibly because the feature is still new.

In order to adopt GitHub Projects as a common kanban for a group project, I decided it would help immensely to build an relayer myself, basically acting as a proxy for webhooks.

## Flow

1. GitHub pushes a webhook event to the server running `gh-projects-discord-exe` (port 3000).
2. The service parses and optionally fetches more information relating to the Project board.
3. The service pushes a webhook event to Discord using the webhook URL passed in as the cli argument.

## License

[MIT](./ChangeLog.md)
