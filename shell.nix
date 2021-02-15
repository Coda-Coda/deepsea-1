{ pkgs ? import <nixpkgs> {} }:

# Run nix-build before nix-shell, to first generate the edsger binary

pkgs.mkShell {
  buildInputs = [ 
    (import ./dependencies.nix)
    ];

  shellHook = ''
    export PATH=$PATH:$PWD/result/bin/
  '';
    
}