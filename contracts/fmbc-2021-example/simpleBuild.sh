# Requires Coq 8.9.1 https://github.com/coq/coq/releases/tag/V8.9.1

pushd ../../src/
make
popd
cd contract/
coqdep -f _CoqProject > .coqdeps.d
coq_makefile -f _CoqProject -o core.make 
make -f core.make