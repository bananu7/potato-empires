# Potato Empires #

Potato Empires is an turn-based strategy game.

## Installation ##

### The Client ###
The client is a standalone local file at this moment.

### The Server ###
* Get the latest Haskell Platform (2014.02 at the time of writing)
* Navigate to the `potato-server` directory and run:
    * (optionally) `cabal sandbox init`
    * `cabal install --dependencies-only`
    * `cabal configure`
    * `cabal run`

Sorry for the inconvenience caused, but `cabal install` doesn't work right now for whatever reason.