let pkgs20-09PinnedUrl = https://github.com/nixos/nixpkgs/archive/5e199f944cfa599636ff93e14d27e1d08ad2d9d1.tar.gz; in
let pkgs = import (fetchTarball pkgs20-09PinnedUrl) {}; in
let
  nodePackages = import ./node-packages {
    inherit pkgs;
  };
in 
let dependencies = (with pkgs; [
    coq_8_9
    ocamlPackages.core
    ocamlPackages.dune_2
    ocamlPackages.ocaml
    ocamlPackages.findlib
    ocamlPackages.utop
    ocamlPackages.cryptokit
    ocamlPackages.ocamlbuild
    ocamlPackages.cppo
    ocamlPackages.ocaml_extlib
    ocamlPackages.yojson
    ocamlPackages.menhir
    ocamlPackages.zarith
    ncurses
    ]);
in
dependencies