## About this fork
The main branch of this (unofficial) fork includes various improvements not yet merged upstream. For example, this version is compatible with Coq 8.14 (rather than the original repository which uses Coq 8.9). Please feel free to examine the git history to see the other changes since the repositories diverged at [commit 603866](https://github.com/shentufoundation/deepsea/commit/60386608f23e5e316196ac73db417007783240d8).

# The DeepSEA Language

This codebase **may only be used for educational, research or evaluation purposes, and not for commercial use**.
This is because the DeepSEA compiler includes files taken and modified from CompCert, so it is developed pursuant to the CompCert licence. Please see the [CompCert licence](./CompCert-LICENSE.txt) for the full details. You may also wish to look at the [INRIA CompCert research project website](https://compcert.org) or the [CompCert GitHub repository](https://github.com/AbsInt/CompCert).  

[The Shentu Chain webpage](https://www.shentu.technology/technology#deepsea)
has more information about the DeepSEA project, and there are blog posts [An Introduction to DeepSEA](https://www.certik.com/resources/blog/an-introduction-to-deepsea) and [How DeepSEA Works](https://www.certik.com/resources/blog/how-deepsea-works-with-an-example-token-contact) (see also the [version on the Internet Archive](https://web.archive.org/web/20211020184322/https://www.certik.io/blog/technology/how-deepsea-works-with-an-example-token-contact/)).

Please let us know what you think! Feedback can be sent to deepsea@certik.org or for feedback specific to this fork, feel free to [raise an issue](https://github.com/Coda-Coda/deepsea-1/issues/new). 

-----


This directory contains the OCaml parts of the DeepSEA compiler. For the
time being, we do not include the sources for the parts that are written in
Coq, but we ship the Ocaml files which were compiled from them.

In order to build it, use opam or Nix to install the prerequisites, and then run make:

```
opam install .
make edsger
mv _build/default/Edsger/edsger.bc dsc
```

OR

```
nix-shell
make edsger
mv _build/default/Edsger/edsger.bc dsc
```


To install the dependencies using Nix, run `nix-shell` from the root of the repository to install most dependencies without `opam` (install `npm` packages separately). Nix is a package manager available on Linux, macOS, and (via WSL) Windows. You can [get Nix here](https://nixos.org/guides/install-nix.html).