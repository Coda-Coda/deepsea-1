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

Lemma Contract_safeExample_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_safeExample Contract_safeExample_wf
                    me d.
Proof.
Admitted.

Lemma Contract_safeExample_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_safeExample Contract_safeExample_wf
                          me d.
Proof.
Admitted.

Lemma Contract_unsafeExample_vc me d :
    high_level_invariant d ->
    synth_func_cond Contract_unsafeExample Contract_unsafeExample_wf
                    me d.
Proof.
Admitted.

Lemma Contract_unsafeExample_oblg me d :
    high_level_invariant d ->
    synth_func_obligation Contract_unsafeExample Contract_unsafeExample_wf
                          me d.
Proof.
Admitted.
TODO-here
End EdsgerGen.
