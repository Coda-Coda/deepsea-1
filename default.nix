let pkgs = import (builtins.fetchTarball {
  name = "nixos-20.09-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/5e199f944cfa599636ff93e14d27e1d08ad2d9d1.tar.gz";
  sha256 = "15z10ql82qscv3cc2l1kvqb3s9pgip1kc500szimgvzvj0fzzbll";
}) {}; in
pkgs.stdenv.mkDerivation {
  name = "dsc-and-deepsea-coq-built";
  buildInputs = (import ./dependencies.nix);

  src = ./src;

  postPatch = ''
    patchShebangs .
  '';

  buildPhase = ''
    make
    make --always-make Edsger/parser.ml
    make edsger
  '';

  installPhase = ''
    mkdir -p $out

    echo "These files are built from https://github.com/Coda-Coda/deepsea-1 which is a fork of https://github.com/certikfoundation/deepsea/" > $out/README-result.md
    
    #cp CompCert-LICENSE.txt $out/CompCert-LICENSE.txt
    #cp Selected-Files-LICENSE.txt $out/Selected-Files-License.txt

    #cp dependencies.nix $out/dependencies.nix
    
    #cp -r src/ $out/src/

    mkdir -p $out/binaries/built-from-src
    cp ./_build/default/Edsger/edsger.bc $out/binaries/built-from-src/dsc
    chmod +x $out/binaries/built-from-src/dsc
  '';
}