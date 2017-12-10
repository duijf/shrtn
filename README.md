# shrtn

Currently in development. Alpha quality software, everything subject to change.

An as simple as possible to manage URL shortner. Project goals:

 - Easy to operate and install -- Just copy our binary to your server or use
   one of our packages.
 - Does not depend on random DBs -- Persistent state is saved to a file (with
   some reasonably ACID guarantees).
 - Support authentication before creating shortened URLs.
 - Ability to choose your own URL -- Many available shortners only give you a
   random URL.
 - Can function standalone -- Manages Let's Encrypt certs for you

## Development setup

Shrtn is written in Haskell and can be built using the [`stack`][stack] tool.
Build and run instructions:

```shell
# Get the code using git
$ git clone https://github.com:duijf/shrtn.git
$ cd shrtn

# Create a build (installs a Haskell compiler if needed)
$ stack build --install-ghc

# Run the shrtn binary from your terminal. shrtn serves redirects from port
# 7000 and a management API from port 7001.
$ stack exec shrtn
```

 [stack]:https://docs.haskellstack.org/en/stable/README/

## Deployment

Don't do this yet.
