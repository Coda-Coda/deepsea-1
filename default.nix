{ pkgs ? import <nixpkgs> {} } :
pkgs.stdenv.mkDerivation {
  name = "dsc";
  buildInputs = (import ./dependencies.nix);

  src = ./.;

  postPatch = ''
    patchShebangs .
  '';

  buildPhase = ''
    cd src
    make
    make --always-make Edsger/parser.ml
    make edsger
    cd ..
  '';

  installPhase = ''
    mkdir -p $out/DeepSEA
    cp -r src/. $out/DeepSEA/

    mkdir -p $out/bin
    cp src/_build/default/Edsger/edsger.bc $out/bin/dsc
    chmod +x $out/bin/dsc

    cp dependencies.nix $out/dependencies.nix
  '';
}
