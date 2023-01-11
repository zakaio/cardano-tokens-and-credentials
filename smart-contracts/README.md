#


### Cabal+Nix build

Alternatively, use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

Otherwise, you can use [direnv](https://github.com/direnv/direnv) which allows you to use your preferred shell. Once installed, just run:

```
$ echo "use nix" > .envrc # Or manually add "use nix" in .envrc if you already have one
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.



## How to start Script generator

```
cabal build proofspace-token-credentials-gen
cp ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/proofspace-token-credentials-0.1.0.0/x/proofspace-token-credentials-gen/opt/build/proofspace-token-credentials-gen/proofspace-token-credentials-gen 
```

## How to start PAB


1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````

This will then start up the server on port 9080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:9080/api/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

