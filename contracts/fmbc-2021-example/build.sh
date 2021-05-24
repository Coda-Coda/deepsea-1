# The Nix package manager is required to run this script.
# To get Nix go to: https://nixos.org/download.html
# Or just run: curl -L https://nixos.org/nix/install | sh
# macOS and Linux only (though it might work with WSL on Windows).
# You can also use opam (all OSes) for the dependencies instead (see src/README.md). In that case dsc may instead be called edsger.bc when built.
nix-build
nix-shell --run "
    dsc contract.ds coq
    cd contract
    coqdep -f _CoqProject > .coqdeps.d
    coq_makefile -f _CoqProject -o core.make 
    make -f core.make
"