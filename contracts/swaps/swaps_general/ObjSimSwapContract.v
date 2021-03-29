(* WARNING: This file is generated by Edsger, the DeepSEA compiler.
            All modification will be lost when regenerating. *)
(* Module swaps_general.ObjSimSwapContract for swaps_general.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import swaps_general.EdsgerIdents.
Require Import swaps_general.DataTypes.
Require Import swaps_general.DataTypeOps.
Require Import swaps_general.DataTypeProofs.
Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.
Require Import layerlib.RefinementTactic.
Require Import layerlib.RefinementTacticAux.

Require Import swaps_general.LayerSWAPCONTRACT.
Require Import swaps_general.LSimEVMOPCODE.

Section EdsgerGen.


Context {Hmem: Mem.MemoryModel mem}.
Context`{Hmwd: UseMemWithData mem}.
Context`{make_program_ops: !MakeProgramOps Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Context`{Hmake_program: !MakeProgram Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Instance GlobalLayerSpec : LayerSpecClass := {
  make_program_ops := make_program_ops;
  Hmake_program := Hmake_program;
  GetHighData := global_abstract_data_type
}.
Context`{HM : HyperMem (LayerSpec := GlobalLayerSpec)}.

  Context {EVMOpcode__events_var_pt_prf : variable_passthrough_prf EVMOpcode__events_var}.
  Context {DiGraph__leaders_var_pt_prf : variable_passthrough_prf DiGraph__leaders_var}.
  Context {DiGraph__parties_var_pt_prf : variable_passthrough_prf DiGraph__parties_var}.
  Context {DiGraph__edges_var_pt_prf : variable_passthrough_prf DiGraph__edges_var}.
  Context {DiGraph__edges_reverse_var_pt_prf : variable_passthrough_prf DiGraph__edges_reverse_var}.
  Context {DiGraph__party_addr_to_ind_var_pt_prf : variable_passthrough_prf DiGraph__party_addr_to_ind_var}.
  Context {DiGraph__leader_addr_to_ind_var_pt_prf : variable_passthrough_prf DiGraph__leader_addr_to_ind_var}.
  Context {DiGraph__party_var_pt_prf : variable_passthrough_prf DiGraph__party_var}.
  Context {DiGraph__counterparty_var_pt_prf : variable_passthrough_prf DiGraph__counterparty_var}.
  Context {DiGraph__hashkeyinit_var_pt_prf : variable_passthrough_prf DiGraph__hashkeyinit_var}.
  Context {DiGraph__innerpathinit_var_pt_prf : variable_passthrough_prf DiGraph__innerpathinit_var}.
  Context {DiGraph__allpaths_var_pt_prf : variable_passthrough_prf DiGraph__allpaths_var}.
  Context {DiGraph__res_is_leader_var_pt_prf : variable_passthrough_prf DiGraph__res_is_leader_var}.
  Context {DiGraph__res_is_party_var_pt_prf : variable_passthrough_prf DiGraph__res_is_party_var}.
  Context {DiGraph__res_is_edge_var_pt_prf : variable_passthrough_prf DiGraph__res_is_edge_var}.
  Context {DiGraph__res_has_signed_var_pt_prf : variable_passthrough_prf DiGraph__res_has_signed_var}.
  Context {DiGraph__res_can_sign_var_pt_prf : variable_passthrough_prf DiGraph__res_can_sign_var}.
  Context {DiGraph__res_path_len_var_pt_prf : variable_passthrough_prf DiGraph__res_path_len_var}.
  Context {SwapContract__assetAmount_var_pt_prf : variable_passthrough_prf SwapContract__assetAmount_var}.
  Context {SwapContract__timelocks_var_pt_prf : variable_passthrough_prf SwapContract__timelocks_var}.
  Context {SwapContract__hashlocks_var_pt_prf : variable_passthrough_prf SwapContract__hashlocks_var}.
  Context {SwapContract__party_var_pt_prf : variable_passthrough_prf SwapContract__party_var}.
  Context {SwapContract__counterparty_var_pt_prf : variable_passthrough_prf SwapContract__counterparty_var}.
  Context {SwapContract__start_var_pt_prf : variable_passthrough_prf SwapContract__start_var}.
  Context {SwapContract__delta_var_pt_prf : variable_passthrough_prf SwapContract__delta_var}.
  Context {SwapContract__unlocked_var_pt_prf : variable_passthrough_prf SwapContract__unlocked_var}.
  Context {SwapContract__ended_var_pt_prf : variable_passthrough_prf SwapContract__ended_var}.
  Context {SwapContract__canrefund_var_pt_prf : variable_passthrough_prf SwapContract__canrefund_var}.

  Lemma SwapContract_initialize_passthrough_prf :
      function_constr_passthrough_prf SwapContract_initialize.
  Proof. solve_passthrough_prf @SwapContract_initialize. Qed.
  Lemma SwapContract_initialize_passthrough_cond :
      synth_func_passthrough_cond SwapContract_initialize SwapContract_initialize_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance SwapContract_initialize_prim_passthrough
      : primitive_passthrough_prf SwapContract_initialize_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, SwapContract_initialize_prim.
      rewrite SwapContract_initialize_spec_hlist_eq.
      exact (synth_func_spec_relate
              SwapContract_initialize_passthrough_prf
              SwapContract_initialize_wf
              SwapContract_initialize_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_initialize_prim.
      rewrite SwapContract_initialize_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              SwapContract_initialize_passthrough_prf
              SwapContract_initialize_wf
              SwapContract_initialize_passthrough_cond).
    (*
    - unfold PRIMsem, SwapContract_initialize_prim.
      rewrite SwapContract_initialize_spec_hlist_eq.
      exact (synth_func_spec_match
              SwapContract_initialize_passthrough_prf
              SwapContract_initialize_wf
              SwapContract_initialize_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_initialize_prim.
      rewrite SwapContract_initialize_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              SwapContract_initialize_passthrough_prf
              SwapContract_initialize_wf
              SwapContract_initialize_passthrough_cond).
  Qed.

  Lemma SwapContract_initialize_exists :
    forall j a0 a1 a2 a3 d1 d2 d1',
      relate_AbData j d1 d2 ->
      SwapContract_initialize_spec a0 a1 a2 a3 d1 = Some d1' ->
      exists d2',
        SwapContract_initialize_spec a0 a1 a2 a3 d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * SwapContract_initialize_spec_eq.
    exact (synth_func_relate_4_void
            SwapContract_initialize_passthrough_prf
            SwapContract_initialize_wf
            SwapContract_initialize_passthrough_cond
            j (fun _ _ x => x) (fun _ _ x => x) (fun _ _ x => x) (fun _ _ x => x)).
  Qed.

  Lemma SwapContract_initialize_match :
    forall j d d' m a0 a1 a2 a3,
      match_AbData d m j ->
      SwapContract_initialize_spec a0 a1 a2 a3 d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * SwapContract_initialize_spec_eq.
    exact (synth_func_match_4_void
            SwapContract_initialize_passthrough_prf
            SwapContract_initialize_wf
            SwapContract_initialize_passthrough_cond).
  Qed.

  Section initialize.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) SwapContract_initialize_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) SwapContract_initialize_spec}.

    Lemma SwapContract_initialize_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem SwapContract_initialize_spec)
            (id ↦ CompatGenSem.gensem SwapContract_initialize_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit SwapContract_initialize_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply SwapContract_initialize_match; eauto.
    Qed.
  End initialize.

  Lemma SwapContract_unlock_passthrough_prf :
      function_constr_passthrough_prf SwapContract_unlock.
  Proof. solve_passthrough_prf @SwapContract_unlock. Qed.
  Lemma SwapContract_unlock_passthrough_cond :
      synth_func_passthrough_cond SwapContract_unlock SwapContract_unlock_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance SwapContract_unlock_prim_passthrough
      : primitive_passthrough_prf SwapContract_unlock_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, SwapContract_unlock_prim.
      rewrite SwapContract_unlock_spec_hlist_eq.
      exact (synth_func_spec_relate
              SwapContract_unlock_passthrough_prf
              SwapContract_unlock_wf
              SwapContract_unlock_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_unlock_prim.
      rewrite SwapContract_unlock_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              SwapContract_unlock_passthrough_prf
              SwapContract_unlock_wf
              SwapContract_unlock_passthrough_cond).
    (*
    - unfold PRIMsem, SwapContract_unlock_prim.
      rewrite SwapContract_unlock_spec_hlist_eq.
      exact (synth_func_spec_match
              SwapContract_unlock_passthrough_prf
              SwapContract_unlock_wf
              SwapContract_unlock_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_unlock_prim.
      rewrite SwapContract_unlock_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              SwapContract_unlock_passthrough_prf
              SwapContract_unlock_wf
              SwapContract_unlock_passthrough_cond).
  Qed.

  Lemma SwapContract_unlock_exists :
    forall j a0 a1 d1 d2 d1',
      relate_AbData j d1 d2 ->
      SwapContract_unlock_spec a0 a1 d1 = Some d1' ->
      exists d2',
        SwapContract_unlock_spec a0 a1 d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * SwapContract_unlock_spec_eq.
    exact (synth_func_relate_2_void
            SwapContract_unlock_passthrough_prf
            SwapContract_unlock_wf
            SwapContract_unlock_passthrough_cond
            j (fun _ _ x => x) (fun _ _ x => x)).
  Qed.

  Lemma SwapContract_unlock_match :
    forall j d d' m a0 a1,
      match_AbData d m j ->
      SwapContract_unlock_spec a0 a1 d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * SwapContract_unlock_spec_eq.
    exact (synth_func_match_2_void
            SwapContract_unlock_passthrough_prf
            SwapContract_unlock_wf
            SwapContract_unlock_passthrough_cond).
  Qed.

  Section unlock.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) SwapContract_unlock_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) SwapContract_unlock_spec}.

    Lemma SwapContract_unlock_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem SwapContract_unlock_spec)
            (id ↦ CompatGenSem.gensem SwapContract_unlock_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit SwapContract_unlock_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply SwapContract_unlock_match; eauto.
    Qed.
  End unlock.

  Lemma SwapContract_claim_passthrough_prf :
      function_constr_passthrough_prf SwapContract_claim.
  Proof. solve_passthrough_prf @SwapContract_claim. Qed.
  Lemma SwapContract_claim_passthrough_cond :
      synth_func_passthrough_cond SwapContract_claim SwapContract_claim_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance SwapContract_claim_prim_passthrough
      : primitive_passthrough_prf SwapContract_claim_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, SwapContract_claim_prim.
      rewrite SwapContract_claim_spec_hlist_eq.
      exact (synth_func_spec_relate
              SwapContract_claim_passthrough_prf
              SwapContract_claim_wf
              SwapContract_claim_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_claim_prim.
      rewrite SwapContract_claim_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              SwapContract_claim_passthrough_prf
              SwapContract_claim_wf
              SwapContract_claim_passthrough_cond).
    (*
    - unfold PRIMsem, SwapContract_claim_prim.
      rewrite SwapContract_claim_spec_hlist_eq.
      exact (synth_func_spec_match
              SwapContract_claim_passthrough_prf
              SwapContract_claim_wf
              SwapContract_claim_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_claim_prim.
      rewrite SwapContract_claim_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              SwapContract_claim_passthrough_prf
              SwapContract_claim_wf
              SwapContract_claim_passthrough_cond).
  Qed.

  Lemma SwapContract_claim_exists :
    forall j d1 d2 d1',
      relate_AbData j d1 d2 ->
      SwapContract_claim_spec d1 = Some d1' ->
      exists d2',
        SwapContract_claim_spec d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * SwapContract_claim_spec_eq.
    exact (synth_func_relate_0_void
            SwapContract_claim_passthrough_prf
            SwapContract_claim_wf
            SwapContract_claim_passthrough_cond
            j).
  Qed.

  Lemma SwapContract_claim_match :
    forall j d d' m,
      match_AbData d m j ->
      SwapContract_claim_spec d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * SwapContract_claim_spec_eq.
    exact (synth_func_match_0_void
            SwapContract_claim_passthrough_prf
            SwapContract_claim_wf
            SwapContract_claim_passthrough_cond).
  Qed.

  Section claim.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) SwapContract_claim_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) SwapContract_claim_spec}.

    Lemma SwapContract_claim_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem SwapContract_claim_spec)
            (id ↦ CompatGenSem.gensem SwapContract_claim_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit SwapContract_claim_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply SwapContract_claim_match; eauto.
    Qed.
  End claim.

  Lemma SwapContract_refund_passthrough_prf :
      function_constr_passthrough_prf SwapContract_refund.
  Proof. solve_passthrough_prf @SwapContract_refund. Qed.
  Lemma SwapContract_refund_passthrough_cond :
      synth_func_passthrough_cond SwapContract_refund SwapContract_refund_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance SwapContract_refund_prim_passthrough
      : primitive_passthrough_prf SwapContract_refund_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, SwapContract_refund_prim.
      rewrite SwapContract_refund_spec_hlist_eq.
      exact (synth_func_spec_relate
              SwapContract_refund_passthrough_prf
              SwapContract_refund_wf
              SwapContract_refund_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_refund_prim.
      rewrite SwapContract_refund_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              SwapContract_refund_passthrough_prf
              SwapContract_refund_wf
              SwapContract_refund_passthrough_cond).
    (*
    - unfold PRIMsem, SwapContract_refund_prim.
      rewrite SwapContract_refund_spec_hlist_eq.
      exact (synth_func_spec_match
              SwapContract_refund_passthrough_prf
              SwapContract_refund_wf
              SwapContract_refund_passthrough_cond).
    *)
    - unfold PRIMsem_opt, SwapContract_refund_prim.
      rewrite SwapContract_refund_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              SwapContract_refund_passthrough_prf
              SwapContract_refund_wf
              SwapContract_refund_passthrough_cond).
  Qed.

  Lemma SwapContract_refund_exists :
    forall j d1 d2 d1',
      relate_AbData j d1 d2 ->
      SwapContract_refund_spec d1 = Some d1' ->
      exists d2',
        SwapContract_refund_spec d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * SwapContract_refund_spec_eq.
    exact (synth_func_relate_0_void
            SwapContract_refund_passthrough_prf
            SwapContract_refund_wf
            SwapContract_refund_passthrough_cond
            j).
  Qed.

  Lemma SwapContract_refund_match :
    forall j d d' m,
      match_AbData d m j ->
      SwapContract_refund_spec d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * SwapContract_refund_spec_eq.
    exact (synth_func_match_0_void
            SwapContract_refund_passthrough_prf
            SwapContract_refund_wf
            SwapContract_refund_passthrough_cond).
  Qed.

  Section refund.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) SwapContract_refund_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) SwapContract_refund_spec}.

    Lemma SwapContract_refund_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem SwapContract_refund_spec)
            (id ↦ CompatGenSem.gensem SwapContract_refund_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit SwapContract_refund_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply SwapContract_refund_match; eauto.
    Qed.
  End refund.

End EdsgerGen.