# Ensure you are in a nix-shell (go to the root of the repo and run nix-shell)
# To get Nix go to: https://nixos.org/download.html
# Or just run: curl -L https://nixos.org/nix/install | sh
# macOS and Linux only (might work with WSL on Windows).
# You can also use opam (all OSes) for the dependencies instead. In that case dsc may instead be called edsger.bc when built.
dsc contract.ds coq
cd contract
 coqdep -f _CoqProject > .coqdeps.d
 coq_makefile -f _CoqProject -o core.make 
 make -f core.make
