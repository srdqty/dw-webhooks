# dw-webhooks

My template for creating new haskell projects using cabal and nix.

## Initialization

First rename the project. This will go through and replace all the
dw-webhooks text to your project name.

```
nix-shell --argstr project-name <your-project-name> nix/scripts/change-project-name.nix
```

This script generates the information necessary for pinning nixpkgs to a known
commit. Useful for reproducible builds. This tends to take a while because
the nixpkgs repo is pretty large. Should only need to run this once unless you
want to change to a different commit sha.

```
nix-shell --pure nix/scripts/generate-nixpkgs-json.nix
```


This script genereates the cabal file and nix file from the hpack yaml file.
Rerurn this whenever you update the hpack yaml file.

```
nix-shell --pure nix/scripts/generate-cabal-and-nix-file.nix
```

## Development

The purpose of this environment is to make available everything that your project
needs to compile, while leaving the project itself unbuilt. Then you can work on
your project and build it using cabal.

```
# Enter the development environment
# This script automatically regenerates the default.nix and cabal files first.

./enter-dev.sh
```

```
cabal build
```

### Repl

```
# Enter the development environment

./enter-dev.sh
```

```
# In the context of the executable build target

cabal repl exe:dw-webhooks
```

```
# In the context of the library build target

cabal repl lib:dw-webhooks
```

### Running Tests

```
# Enter the development environment

./enter-dev.sh
```

```
cabal test
```

### Without entering the development environment

This script does not automatically regenerate the default.nix and cabal files.

```
./cabal.sh build
./cabal.sh repl exe:dw-webhooks
./cabal.sh repl lib:dw-webhooks
./cabal.sh test
```

## Release Build

```
# Build the package

nix-build nix/release.nix

The built executable and library will be placed in ./result
```

```
# Run your executable

./result/bin/dw-webhooks
```

## Docker Image

You can use nix to build a docker image for your project.

```
# Build the image. We don't use pure so we can use the system docker and nix-build.

nix-shell nix/scripts/build-docker-image.nix
```

```
# Run a container

docker run --rm dw-webhooks-image
```

## Different compiler versions

You can either edit `nix/ghc.nix` or specify a compiler version at the command
line as demonstrated below.

```
nix-shell --pure nix/development.nix --argstr compiler ghc802
nix-shell --pure nix/release.nix --argstr compiler ghc802
nix-shell nix/scripts/build-docker-image.nix --argstr compiler ghc802
```
