let pkgs = import (
  builtins.fetchTarball {
  name = "nixpkgs-21.05-pinned";
  url = "https://github.com/nixos/nixpkgs/archive/b199038e38f8b97239d1e80dc373fa9b0fd3194d.tar.gz";
  sha256 = "00iiypj3l8gc295syv00m1f21n8m1hw9rvgxjwjnpdnr1nnwjq5d";
}) {}; in

pkgs.mkShell {
  name = "DeepSEA-env";
  buildInputs = with pkgs; [ 
    (import ./dependencies.nix)
    ];

  shellHook = ''
    export PATH=$PATH:$PWD/scripts/
  '';
}