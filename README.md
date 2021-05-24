Please note: this is an **unofficial** fork of https://github.com/certikfoundation/deepsea/

The code related to the FMBC 2021 Paper is (currently) only available in this repository and not in CertiK's version.

----------

# Files particularly relevant to FMBC 2021 Paper

[contracts/fmbc-2021-example/contract.ds](contracts/fmbc-2021-example/contract.ds) has the DeepSEA smart contract source code used in examples.

[src/core/Syntax.v (starting line 541)](https://github.com/Coda-Coda/deepsea-1/blob/fmbc-2021/src/core/Syntax.v#L541) has the definition of the inductive proposition that defines the notion of a command following the *Checks-Effects-Interactions Pattern*. The definition of `function_constr_CEI_pattern_prf` is also given here ([line 698](https://github.com/Coda-Coda/deepsea-1/blob/fmbc-2021/src/core/Syntax.v#L698)).

[src/Runtime.v (starting line 862)](https://github.com/Coda-Coda/deepsea-1/blob/fmbc-2021/src/Runtime.v#L862) has the definitions of the tactics used to automatically prove that a command follows the *Checks-Effects-Interactions Pattern*.

[src/Edsger/coqgen.ml (starting line 2597)](https://github.com/Coda-Coda/deepsea-1/blob/fmbc-2021/src/Edsger/coqgen.ml#L2597) has the OCaml code which generates the proof obligations related to following the *Checks-Effects-Interactions Pattern* for each command. Obligations relating to calls to other functions within a smart contract (`CCcall`) are also facilitated by this `coqgen.ml` code.

----------

# The DeepSEA Language

You can download a zip file from the [releases
page](https://github.com/CertiKFoundation/deepsea/releases). Please see the [DeepSEA language reference](https://github.com/CertiKFoundation/deepsea/blob/master/DeepSEA%20language%20reference.pdf) for installation and usage instructions.  

The zip file includes pre-built binaries for Linux (Ubuntu) and MacOS. Since they use some system libraries, it is possible that they will not work on every version of Linux or MacOS. It is also easy to build the binaries from source yourself, following the instructions in the `src` directory.

[The project page at the CertiK website](https://certik.io/research/deepsea/)
has more information about the DeepSEA project, including the blog posts [An Introduction to DeepSEA](https://certik.io/blog/technology/an-introduction-to-deepsea) and [How DeepSEA Works](https://certik.io/blog/technology/how-deepsea-works-with-an-example-token-contact/).  

The DeepSEA compiler includes files taken and modified from CompCert, so it is developed pursuant to the CompCert licence. In particular, it may only be used for educational, research, personal or evaluation purposes, and not for commercial use.

Please let us know what you think! Feedback can be sent to deepsea@certik.org. 
