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
    mkdir -p $out
    
    cp CompCert-LICENSE.txt $out/CompCert-LICENSE.txt
    cp Selected-Files-LICENSE.txt $out/Selected-Files-License.txt

    cp dependencies.nix $out/dependencies.nix
    
    cp -r src/ $out/src/

    mkdir -p $out/binaries/built-from-src
    cp src/_build/default/Edsger/edsger.bc $out/binaries/built-from-src/dsc
    chmod +x $out/binaries/built-from-src/dsc
  '';
}