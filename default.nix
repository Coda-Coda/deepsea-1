let pkgs = import (builtins.fetchTarball {
  name = "nixpkgs-21.05-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/61ac4169922ca62d00bfe7f28dae6e1f27fd9c89.tar.gz";
  sha256 = "05rjb4xx2m2qqp94x39k8mv447njvyqv1zk6kshkg0j9q4hcq8lf";
}) {}; in
let dependencies = import (./dependencies.nix); in

pkgs.stdenv.mkDerivation {
  name = "dsc-and-deepsea-coq-built";
  buildInputs = [
    dependencies.other
    dependencies.proving
    dependencies.dsc
  ];

  src = ./.;

  postPatch = ''
    patchShebangs .
  '';

  buildPhase = ''
    make
    make --always-make parser
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