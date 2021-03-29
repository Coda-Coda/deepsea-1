(* WARNING: This file is generated by Edsger, the DeepSEA compiler.
            All modification will be lost when regenerating. *)
(* Module fpblind.ObjSimBlindAuction for fpblind.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import fpblind.EdsgerIdents.
Require Import fpblind.DataTypes.
Require Import fpblind.DataTypeOps.
Require Import fpblind.DataTypeProofs.
Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.
Require Import layerlib.RefinementTactic.
Require Import layerlib.RefinementTacticAux.

Require Import fpblind.LayerBLINDAUCTION.
Require Import fpblind.LSimEVMOPCODE.

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
  Context {BlindAuction__beneficiary_var_pt_prf : variable_passthrough_prf BlindAuction__beneficiary_var}.
  Context {BlindAuction__biddingEnd_var_pt_prf : variable_passthrough_prf BlindAuction__biddingEnd_var}.
  Context {BlindAuction__revealEnd_var_pt_prf : variable_passthrough_prf BlindAuction__revealEnd_var}.
  Context {BlindAuction__ended_var_pt_prf : variable_passthrough_prf BlindAuction__ended_var}.
  Context {BlindAuction__bids_var_pt_prf : variable_passthrough_prf BlindAuction__bids_var}.
  Context {BlindAuction__highestBidder_var_pt_prf : variable_passthrough_prf BlindAuction__highestBidder_var}.
  Context {BlindAuction__highestBid_var_pt_prf : variable_passthrough_prf BlindAuction__highestBid_var}.
  Context {BlindAuction__pendingReturns_var_pt_prf : variable_passthrough_prf BlindAuction__pendingReturns_var}.
  Context {BlindAuction__trueBids_var_pt_prf : variable_passthrough_prf BlindAuction__trueBids_var}.
  Context {BlindAuction__secrets_var_pt_prf : variable_passthrough_prf BlindAuction__secrets_var}.

  Lemma BlindAuction_initialize_passthrough_prf :
      function_constr_passthrough_prf BlindAuction_initialize.
  Proof. solve_passthrough_prf @BlindAuction_initialize. Qed.
  Lemma BlindAuction_initialize_passthrough_cond :
      synth_func_passthrough_cond BlindAuction_initialize BlindAuction_initialize_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance BlindAuction_initialize_prim_passthrough
      : primitive_passthrough_prf BlindAuction_initialize_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, BlindAuction_initialize_prim.
      rewrite BlindAuction_initialize_spec_hlist_eq.
      exact (synth_func_spec_relate
              BlindAuction_initialize_passthrough_prf
              BlindAuction_initialize_wf
              BlindAuction_initialize_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_initialize_prim.
      rewrite BlindAuction_initialize_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              BlindAuction_initialize_passthrough_prf
              BlindAuction_initialize_wf
              BlindAuction_initialize_passthrough_cond).
    (*
    - unfold PRIMsem, BlindAuction_initialize_prim.
      rewrite BlindAuction_initialize_spec_hlist_eq.
      exact (synth_func_spec_match
              BlindAuction_initialize_passthrough_prf
              BlindAuction_initialize_wf
              BlindAuction_initialize_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_initialize_prim.
      rewrite BlindAuction_initialize_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              BlindAuction_initialize_passthrough_prf
              BlindAuction_initialize_wf
              BlindAuction_initialize_passthrough_cond).
  Qed.

  Lemma BlindAuction_initialize_exists :
    forall j a0 a1 d1 d2 d1',
      relate_AbData j d1 d2 ->
      BlindAuction_initialize_spec a0 a1 d1 = Some d1' ->
      exists d2',
        BlindAuction_initialize_spec a0 a1 d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * BlindAuction_initialize_spec_eq.
    exact (synth_func_relate_2_void
            BlindAuction_initialize_passthrough_prf
            BlindAuction_initialize_wf
            BlindAuction_initialize_passthrough_cond
            j (fun _ _ x => x) (fun _ _ x => x)).
  Qed.

  Lemma BlindAuction_initialize_match :
    forall j d d' m a0 a1,
      match_AbData d m j ->
      BlindAuction_initialize_spec a0 a1 d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * BlindAuction_initialize_spec_eq.
    exact (synth_func_match_2_void
            BlindAuction_initialize_passthrough_prf
            BlindAuction_initialize_wf
            BlindAuction_initialize_passthrough_cond).
  Qed.

  Section initialize.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) BlindAuction_initialize_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) BlindAuction_initialize_spec}.

    Lemma BlindAuction_initialize_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem BlindAuction_initialize_spec)
            (id ↦ CompatGenSem.gensem BlindAuction_initialize_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit BlindAuction_initialize_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply BlindAuction_initialize_match; eauto.
    Qed.
  End initialize.

  Lemma BlindAuction_reveal_passthrough_prf :
      function_constr_passthrough_prf BlindAuction_reveal.
  Proof. solve_passthrough_prf @BlindAuction_reveal. Qed.
  Lemma BlindAuction_reveal_passthrough_cond :
      synth_func_passthrough_cond BlindAuction_reveal BlindAuction_reveal_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance BlindAuction_reveal_prim_passthrough
      : primitive_passthrough_prf BlindAuction_reveal_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, BlindAuction_reveal_prim.
      rewrite BlindAuction_reveal_spec_hlist_eq.
      exact (synth_func_spec_relate
              BlindAuction_reveal_passthrough_prf
              BlindAuction_reveal_wf
              BlindAuction_reveal_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_reveal_prim.
      rewrite BlindAuction_reveal_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              BlindAuction_reveal_passthrough_prf
              BlindAuction_reveal_wf
              BlindAuction_reveal_passthrough_cond).
    (*
    - unfold PRIMsem, BlindAuction_reveal_prim.
      rewrite BlindAuction_reveal_spec_hlist_eq.
      exact (synth_func_spec_match
              BlindAuction_reveal_passthrough_prf
              BlindAuction_reveal_wf
              BlindAuction_reveal_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_reveal_prim.
      rewrite BlindAuction_reveal_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              BlindAuction_reveal_passthrough_prf
              BlindAuction_reveal_wf
              BlindAuction_reveal_passthrough_cond).
  Qed.

  Lemma BlindAuction_reveal_exists :
    forall j a0 a1 d1 d2 d1',
      relate_AbData j d1 d2 ->
      BlindAuction_reveal_spec a0 a1 d1 = Some d1' ->
      exists d2',
        BlindAuction_reveal_spec a0 a1 d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * BlindAuction_reveal_spec_eq.
    exact (synth_func_relate_2_void
            BlindAuction_reveal_passthrough_prf
            BlindAuction_reveal_wf
            BlindAuction_reveal_passthrough_cond
            j (fun _ _ x => x) (fun _ _ x => x)).
  Qed.

  Lemma BlindAuction_reveal_match :
    forall j d d' m a0 a1,
      match_AbData d m j ->
      BlindAuction_reveal_spec a0 a1 d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * BlindAuction_reveal_spec_eq.
    exact (synth_func_match_2_void
            BlindAuction_reveal_passthrough_prf
            BlindAuction_reveal_wf
            BlindAuction_reveal_passthrough_cond).
  Qed.

  Section reveal.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) BlindAuction_reveal_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) BlindAuction_reveal_spec}.

    Lemma BlindAuction_reveal_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem BlindAuction_reveal_spec)
            (id ↦ CompatGenSem.gensem BlindAuction_reveal_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit BlindAuction_reveal_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply BlindAuction_reveal_match; eauto.
    Qed.
  End reveal.

  Lemma BlindAuction_bid_passthrough_prf :
      function_constr_passthrough_prf BlindAuction_bid.
  Proof. solve_passthrough_prf @BlindAuction_bid. Qed.
  Lemma BlindAuction_bid_passthrough_cond :
      synth_func_passthrough_cond BlindAuction_bid BlindAuction_bid_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance BlindAuction_bid_prim_passthrough
      : primitive_passthrough_prf BlindAuction_bid_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, BlindAuction_bid_prim.
      rewrite BlindAuction_bid_spec_hlist_eq.
      exact (synth_func_spec_relate
              BlindAuction_bid_passthrough_prf
              BlindAuction_bid_wf
              BlindAuction_bid_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_bid_prim.
      rewrite BlindAuction_bid_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              BlindAuction_bid_passthrough_prf
              BlindAuction_bid_wf
              BlindAuction_bid_passthrough_cond).
    (*
    - unfold PRIMsem, BlindAuction_bid_prim.
      rewrite BlindAuction_bid_spec_hlist_eq.
      exact (synth_func_spec_match
              BlindAuction_bid_passthrough_prf
              BlindAuction_bid_wf
              BlindAuction_bid_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_bid_prim.
      rewrite BlindAuction_bid_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              BlindAuction_bid_passthrough_prf
              BlindAuction_bid_wf
              BlindAuction_bid_passthrough_cond).
  Qed.

  Lemma BlindAuction_bid_exists :
    forall j a0 d1 d2 d1',
      relate_AbData j d1 d2 ->
      BlindAuction_bid_spec a0 d1 = Some d1' ->
      exists d2',
        BlindAuction_bid_spec a0 d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * BlindAuction_bid_spec_eq.
    exact (synth_func_relate_1_void
            BlindAuction_bid_passthrough_prf
            BlindAuction_bid_wf
            BlindAuction_bid_passthrough_cond
            j (fun _ _ x => x)).
  Qed.

  Lemma BlindAuction_bid_match :
    forall j d d' m a0,
      match_AbData d m j ->
      BlindAuction_bid_spec a0 d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * BlindAuction_bid_spec_eq.
    exact (synth_func_match_1_void
            BlindAuction_bid_passthrough_prf
            BlindAuction_bid_wf
            BlindAuction_bid_passthrough_cond).
  Qed.

  Section bid.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) BlindAuction_bid_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) BlindAuction_bid_spec}.

    Lemma BlindAuction_bid_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem BlindAuction_bid_spec)
            (id ↦ CompatGenSem.gensem BlindAuction_bid_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit BlindAuction_bid_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply BlindAuction_bid_match; eauto.
    Qed.
  End bid.

  Lemma BlindAuction_withdraw_passthrough_prf :
      function_constr_passthrough_prf BlindAuction_withdraw.
  Proof. solve_passthrough_prf @BlindAuction_withdraw. Qed.
  Lemma BlindAuction_withdraw_passthrough_cond :
      synth_func_passthrough_cond BlindAuction_withdraw BlindAuction_withdraw_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance BlindAuction_withdraw_prim_passthrough
      : primitive_passthrough_prf BlindAuction_withdraw_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, BlindAuction_withdraw_prim.
      rewrite BlindAuction_withdraw_spec_hlist_eq.
      exact (synth_func_spec_relate
              BlindAuction_withdraw_passthrough_prf
              BlindAuction_withdraw_wf
              BlindAuction_withdraw_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_withdraw_prim.
      rewrite BlindAuction_withdraw_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              BlindAuction_withdraw_passthrough_prf
              BlindAuction_withdraw_wf
              BlindAuction_withdraw_passthrough_cond).
    (*
    - unfold PRIMsem, BlindAuction_withdraw_prim.
      rewrite BlindAuction_withdraw_spec_hlist_eq.
      exact (synth_func_spec_match
              BlindAuction_withdraw_passthrough_prf
              BlindAuction_withdraw_wf
              BlindAuction_withdraw_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_withdraw_prim.
      rewrite BlindAuction_withdraw_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              BlindAuction_withdraw_passthrough_prf
              BlindAuction_withdraw_wf
              BlindAuction_withdraw_passthrough_cond).
  Qed.

  Lemma BlindAuction_withdraw_exists :
    forall j d1 d2 d1',
      relate_AbData j d1 d2 ->
      BlindAuction_withdraw_spec d1 = Some d1' ->
      exists d2',
        BlindAuction_withdraw_spec d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * BlindAuction_withdraw_spec_eq.
    exact (synth_func_relate_0_void
            BlindAuction_withdraw_passthrough_prf
            BlindAuction_withdraw_wf
            BlindAuction_withdraw_passthrough_cond
            j).
  Qed.

  Lemma BlindAuction_withdraw_match :
    forall j d d' m,
      match_AbData d m j ->
      BlindAuction_withdraw_spec d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * BlindAuction_withdraw_spec_eq.
    exact (synth_func_match_0_void
            BlindAuction_withdraw_passthrough_prf
            BlindAuction_withdraw_wf
            BlindAuction_withdraw_passthrough_cond).
  Qed.

  Section withdraw.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) BlindAuction_withdraw_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) BlindAuction_withdraw_spec}.

    Lemma BlindAuction_withdraw_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem BlindAuction_withdraw_spec)
            (id ↦ CompatGenSem.gensem BlindAuction_withdraw_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit BlindAuction_withdraw_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply BlindAuction_withdraw_match; eauto.
    Qed.
  End withdraw.

  Lemma BlindAuction_auctionEnd_passthrough_prf :
      function_constr_passthrough_prf BlindAuction_auctionEnd.
  Proof. solve_passthrough_prf @BlindAuction_auctionEnd. Qed.
  Lemma BlindAuction_auctionEnd_passthrough_cond :
      synth_func_passthrough_cond BlindAuction_auctionEnd BlindAuction_auctionEnd_wf.
  Proof.
    (* XXX: Only works if no [CCrespec], [CCrespec_opt], or [ECrespec] is used. *)
    compute.
    exact I.
  Qed.

  Global Instance BlindAuction_auctionEnd_prim_passthrough
      : primitive_passthrough_prf BlindAuction_auctionEnd_prim.
  Proof. esplit.
    (*
    - unfold PRIMsem, BlindAuction_auctionEnd_prim.
      rewrite BlindAuction_auctionEnd_spec_hlist_eq.
      exact (synth_func_spec_relate
              BlindAuction_auctionEnd_passthrough_prf
              BlindAuction_auctionEnd_wf
              BlindAuction_auctionEnd_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_auctionEnd_prim.
      rewrite BlindAuction_auctionEnd_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_relate
              BlindAuction_auctionEnd_passthrough_prf
              BlindAuction_auctionEnd_wf
              BlindAuction_auctionEnd_passthrough_cond).
    (*
    - unfold PRIMsem, BlindAuction_auctionEnd_prim.
      rewrite BlindAuction_auctionEnd_spec_hlist_eq.
      exact (synth_func_spec_match
              BlindAuction_auctionEnd_passthrough_prf
              BlindAuction_auctionEnd_wf
              BlindAuction_auctionEnd_passthrough_cond).
    *)
    - unfold PRIMsem_opt, BlindAuction_auctionEnd_prim.
      rewrite BlindAuction_auctionEnd_spec_hlist_opt_eq.
      exact (synth_func_spec_opt_match
              BlindAuction_auctionEnd_passthrough_prf
              BlindAuction_auctionEnd_wf
              BlindAuction_auctionEnd_passthrough_cond).
  Qed.

  Lemma BlindAuction_auctionEnd_exists :
    forall j d1 d2 d1',
      relate_AbData j d1 d2 ->
      BlindAuction_auctionEnd_spec d1 = Some d1' ->
      exists d2',
        BlindAuction_auctionEnd_spec d2 = Some d2' /\
        relate_AbData j d1' d2'.
  Proof.
    intro j.
    rewrite * BlindAuction_auctionEnd_spec_eq.
    exact (synth_func_relate_0_void
            BlindAuction_auctionEnd_passthrough_prf
            BlindAuction_auctionEnd_wf
            BlindAuction_auctionEnd_passthrough_cond
            j).
  Qed.

  Lemma BlindAuction_auctionEnd_match :
    forall j d d' m,
      match_AbData d m j ->
      BlindAuction_auctionEnd_spec d = Some d' ->
      match_AbData d' m j.
  Proof.
    rewrite * BlindAuction_auctionEnd_spec_eq.
    exact (synth_func_match_0_void
            BlindAuction_auctionEnd_passthrough_prf
            BlindAuction_auctionEnd_wf
            BlindAuction_auctionEnd_passthrough_cond).
  Qed.

  Section auctionEnd.
    Context`{inv : !CompatGenSem.PreservesInvariants (HD := cdataHigh) BlindAuction_auctionEnd_spec}.
    Context`{inv0 : !CompatGenSem.PreservesInvariants (HD := cdataLow) BlindAuction_auctionEnd_spec}.

    Lemma BlindAuction_auctionEnd_sim :
      forall id,
        sim (crel (CompatRel0 := crel_prf (compatrel := Hcompatrel)) _ _)
            (id ↦ CompatGenSem.gensem BlindAuction_auctionEnd_spec)
            (id ↦ CompatGenSem.gensem BlindAuction_auctionEnd_spec).
    Proof.
      (* non-pure C-primitives *)
      intros. layer_sim_simpl. compatsim_simpl (@match_AbData). intros.
      try inv_semof H0.
      try (inversion H0; subst; simpl in H0).
      exploit BlindAuction_auctionEnd_exists; eauto 1; intros (labd' & HP & rel).
      match_external_states_csem; auto;
      eapply BlindAuction_auctionEnd_match; eauto.
    Qed.
  End auctionEnd.

End EdsgerGen.