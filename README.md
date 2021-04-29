# gh-projects-discord

Relays `GitHub Projects` webhooks to `Discord`. Written in Haskell to have fun.

## Existing GitHub-Discord integration

Using the `/discord` endpoint, most common webhooks like "issue created" or "commit pushed" get shown as Discord embeds in the directed channel. However, webhooks related to GitHub Projects are entirely ignored. This may be because the feature is still new, however it considerably limits the adoption of Projects as a central kanban board in team projects.

I decided it would help a lot if I build a relayer myself, basically acting as a proxy for webhooks.

## Flow

1. GitHub pushes a webhook event to the server running `gh-projects-discord-exe` (port 3000).
2. The service parses and optionally fetches more information relating to the Project board.
3. The service pushes a webhook event to Discord using the webhook URL passed in as the cli argument.

![Demo Image](https://i.imgur.com/OfVpXv1.png)

## Run

1. Create a `gh.token` file containing `<github-username>:<access token>`
2. Run `stack run -- gh-projects-discord-exe "https://discord/webhook/url..."`

## License

[MIT](./LICENSE)
