let pkgs = import (
  builtins.fetchTarball {
  name = "nixpkgs-21.05-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/b199038e38f8b97239d1e80dc373fa9b0fd3194d.tar.gz";
  sha256 = "00iiypj3l8gc295syv00m1f21n8m1hw9rvgxjwjnpdnr1nnwjq5d";
}) {}; in

let dependencies = (import ./dependencies.nix); in

pkgs.mkShell {
  name = "DeepSEA-env";
  buildInputs = with pkgs; [ 
      dependencies.other
      dependencies.proving
      dependencies.dsc
      dependencies.documentation
      dependencies.unittests
      dependencies.conflux-unittests    
    ];

  shellHook = ''
    export PATH=$PATH:$PWD/scripts/
  '';
}