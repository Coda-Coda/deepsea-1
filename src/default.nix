{ pkgs ? import <nixpkgs> {} } :
pkgs.stdenv.mkDerivation {
  name = "dsc";
  buildInputs = (import ./dependencies.nix);

  src = ./.;

  buildPhase = ''
    make
    make edsger
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp -r _build/default/Edsger/edsger.bc $out/bin/dsc
    chmod +x $out/bin/dsc
  '';
}
