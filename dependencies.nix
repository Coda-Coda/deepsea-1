let pkgs = import (
  builtins.fetchTarball {
  name = "nixpkgs-21.05-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/f540aeda6f677354f1e7144ab04352f61aaa0118.tar.gz";
  sha256 = "111x41crq2kyx62a5mrqfk3f0r3m4i4p6dmj4jbpfjn5cdsgbxsr";
}) {}; in

{
  other = (with pkgs; [
    gnumake
    git
    ncurses
    (pkgs.writeShellScriptBin "gsed" "exec -a $0 ${gnused}/bin/sed $@")
  ]);
  proving = (with pkgs; [
    coq_8_14
  ]);
  dsc = (with pkgs; [
    # For dsc/Edsger
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
    ]);
  documentation = (with pkgs; 
    [ mkdocs ]);
  unittests = (with pkgs; [
    nodejs-16_x-openssl_1_1
  ]);
  conflux-unittests = (with pkgs; [
    # For conflux tests for the ANT blockchain
    cargo 
    openssl
    cmake
    pkg-config
  ]);
}