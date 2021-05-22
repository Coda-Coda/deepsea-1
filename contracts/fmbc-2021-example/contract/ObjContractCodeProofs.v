(* Skeleton by Edgser for contract.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import contract.EdsgerIdents.
Require Import contract.DataTypes.
Require Import contract.DataTypeOps.
Require Import contract.DataTypeProofs.
Require Import contract.LayerCONTRACT.

Section EdsgerGen.

Existing Instance GlobalLayerSpec.
Existing Instances CONTRACT_overlay_spec.

Context {memModelOps : MemoryModelOps mem}.

Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
Lemma Contract_constructor_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_constructor Contract_constructor_wf
                    me d.
Proof.
Admitted.

Lemma Contract_constructor_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_constructor Contract_constructor_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_ok Contract_ceip_example_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_ok Contract_ceip_example_ok_wf
                          me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                    me d.
Proof.
Admitted.

Lemma Contract_ceip_example_not_ok_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_ceip_example_not_ok Contract_ceip_example_not_ok_wf
                          me d.
Proof.
Admitted.
TODO-here
End EdsgerGen.
