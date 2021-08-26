let pkgs = import (
  builtins.fetchTarball {
  name = "nixpkgs-21.05-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/b199038e38f8b97239d1e80dc373fa9b0fd3194d.tar.gz";
  sha256 = "00iiypj3l8gc295syv00m1f21n8m1hw9rvgxjwjnpdnr1nnwjq5d";
}) {}; in

pkgs.mkShell {
  name = "DeepSEA-env";
  buildInputs = with pkgs; [ 
    coq_8_9
    nodejs
    ncurses
    git
    gnumake
    mkdocs
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
    ];

  shellHook = ''
    export PATH=$PATH:$PWD/scripts/
  '';
}