{ pkgs ? import <nixpkgs> {} } :
pkgs.stdenv.mkDerivation {
  name = "dsc";
  buildInputs = (import ./dependencies.nix);

  src = ./.;

  postPatch = ''
    patchShebangs .
  '';

  buildPhase = ''
    make
    make --always-make Edsger/parser.ml
    make edsger
  '';

  installPhase = ''
    mkdir -p $out/DeepSEA
    cp -r backend $out/DeepSEA/backend
    cp -r cclib $out/DeepSEA/cclib
    cp -r core $out/DeepSEA/core
    cp -r lib $out/DeepSEA/lib
    cp Runtime.v $out/DeepSEA/Runtime.v
    cp Runtime.glob $out/DeepSEA/Runtime.glob
    cp Runtime.vo $out/DeepSEA/Runtime.vo

    mkdir -p $out/bin
    cp _build/default/Edsger/edsger.bc $out/bin/dsc
    chmod +x $out/bin/dsc
  '';
}
