# Nix instructions for DeepSEA

For the non-Nix README, see [here](README.md).
This README only describes how to use [Nix](https://nixos.org/guides/install-nix.html) to setup the dependencies.

## tldr
1. Get Nix at https://nixos.org/guides/install-nix.html
2. `nix-shell` (in this directory: `src`)
3. `make` (in this directory: `src`)
4. `nix-build` (in this directory: `src`)

## Setup / Dependencies

Install the Nix package manager: https://nixos.org/guides/install-nix.html

## Building dsc

From the src directory, run `nix-build`.

Note that if a `_build` directory already exists the build may fail, first delete `_build`.

## Usage

From the DeepSEA directory, run `nix-shell`. Then you can run `make`, or `dsc` (as long as you have previously run `nix-build`).

-------

Note: the fastest approach to initially get set up for interactive proving is to run `nix-shell` then `make` then `nix-build` to avoid compiling all the Coq files twice as `nix-build` doesn't affect files in the current directory except for adding the `result` symlink.