let pkgs = import (builtins.fetchTarball {
  name = "nixos-20.09-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/5e199f944cfa599636ff93e14d27e1d08ad2d9d1.tar.gz";
  sha256 = "15z10ql82qscv3cc2l1kvqb3s9pgip1kc500szimgvzvj0fzzbll";
}) {}; in
let
  nodePackages = import ./node-packages {
    inherit pkgs;
  };
in 
let dependencies = (with pkgs; [
    coq_8_9
    ocaml-ng.ocamlPackages_4_09.core
    ocaml-ng.ocamlPackages_4_09.dune_2
    ocaml-ng.ocamlPackages_4_09.ocaml
    ocaml-ng.ocamlPackages_4_09.findlib
    ocaml-ng.ocamlPackages_4_09.utop
    ocaml-ng.ocamlPackages_4_09.cryptokit
    ocaml-ng.ocamlPackages_4_09.ocamlbuild
    ocaml-ng.ocamlPackages_4_09.cppo
    ocaml-ng.ocamlPackages_4_09.ocaml_extlib
    ocaml-ng.ocamlPackages_4_09.yojson
    ocaml-ng.ocamlPackages_4_09.menhir
    ocaml-ng.ocamlPackages_4_09.zarith
    ncurses
    git
    ]);
in
dependencies