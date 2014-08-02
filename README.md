# hbot

A simple Haskell chat bot for Hipchat.

## Running

To hack on hbot and run in development it's best to use a cabal sandbox. Make
sure that you have an up to date Cabal, and then:

```bash
$ cabal sandbox init
$ cabal install
```

To run hbot you need to tell it the OAuth2 token for connecting to the Hipchat
API.

```bash
$ AUTH_TOKEN="abc123xyz" dist/dist-sandbox-*/build/hbot/hbot
```

