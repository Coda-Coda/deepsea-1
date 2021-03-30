(* WARNING: This file is generated by Edsger, the DeepSEA compiler.
            All modification will be lost when regenerating. *)
(* Module fpblind.ObjBlindAuctionCode for fpblind.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import DeepSpec.Linking.
Require Import fpblind.EdsgerIdents.
Require Import fpblind.DataTypes.
Require Import fpblind.DataTypeOps.
Require Import fpblind.DataTypeProofs.
Require Import liblayers.compcertx.MakeProgram.
Require Import liblayers.compcertx.MemWithData.
Require Import layerlib.LinkSourceTemplate.

Require Import fpblind.LayerBLINDAUCTION.
Require Import fpblind.RefineBLINDAUCTION.
Require Import fpblind.ObjBlindAuctionCodeProofs.

Section EdsgerGen.


Context {mem}`{Hmem: Mem.MemoryModel mem}.
Context`{Hmwd: UseMemWithData mem}.
Context`{make_program_ops: !MakeProgramOps Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Context`{Hmake_program: !MakeProgram Clight.function Ctypes.type Clight.fundef Ctypes.type}.
Instance GlobalLayerSpec : LayerSpecClass := {
  make_program_ops := make_program_ops;
  Hmake_program := Hmake_program;
  GetHighData := global_abstract_data_type
}.
Context`{global_abdata : !GlobalAbData init_global_abstract_data global_low_level_invariant}.
Context`{CTXT_prf : !Layer_BLINDAUCTION_Context_prf}.
Context`{EVMOPCODE_pres_inv : !EVMOPCODE_preserves_invariants}.
Existing Instances BLINDAUCTION_overlay_spec BLINDAUCTION_underlay_spec
                   BLINDAUCTION_hypermem.

Lemma relate_AbData_translates_kernel_mode j d1 d2 :
    relate_AbData j d1 d2 -> kernel_mode d1 -> kernel_mode d2.
Proof.
  simpl; intros rel d1_kern.
  (* rewrite <- (ikern_re _ _ _ rel), <- (ihost_re _ _ _ rel). *)
  exact d1_kern.
Qed.

Instance EVMOPCODE_evm_transfer_prim_prf :
    primitive_prf EVMOPCODE_evm_transfer_prim.
Proof. esplit; [|
    | intros _;
      set (sem := gencsem EVMOpcode_transfer_spec);
      apply mk_prim_exec_prf with sem
    | discriminate |].
  - (* PRIMsem_opt_simulates_sem *) admit.
  - (* PRIMret_cond *) admit.
  - (* PRIMargs_cast *)
    simpl.
    repeat match goal with
      | |- HList _ _ => constructor
      | |- HyperArgRet (tp_type_pair _) => simpl
      | |- HyperArgRet _ => typeclasses eauto
      end.
  - (* PRIMreturns_cast *) simpl; typeclasses eauto || constructor.
  - (* PRIMget_layer_primitive *)
    reflexivity.
  - (* PRIMsextcall_step *)
    admit.
  - (* PRIMprimsem_sig *)
    reflexivity.
  - (* PRIMmake_external *)
    reflexivity.
  - (* PRIMis_pure *) discriminate.
Qed.

Instance BlindAuction__beneficiary_var_prf :
    HyperLType BlindAuction__beneficiary_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__beneficiary_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__biddingEnd_var_prf :
    HyperLType BlindAuction__biddingEnd_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__biddingEnd_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__revealEnd_var_prf :
    HyperLType BlindAuction__revealEnd_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__revealEnd_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__ended_var_prf :
    HyperLType BlindAuction__ended_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__ended_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__bids_var_prf :
    HyperLType BlindAuction__bids_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__bids_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__highestBidder_var_prf :
    HyperLType BlindAuction__highestBidder_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__highestBidder_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__highestBid_var_prf :
    HyperLType BlindAuction__highestBid_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__highestBid_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__pendingReturns_var_prf :
    HyperLType BlindAuction__pendingReturns_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__pendingReturns_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__trueBids_var_prf :
    HyperLType BlindAuction__trueBids_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__trueBids_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

Instance BlindAuction__secrets_var_prf :
    HyperLType BlindAuction__secrets_var.
Proof. esplit; [ typeclasses eauto | intros _; esplit | discriminate ].
  - (* ltype_writable *)
    intros j d m mm; apply mm.
  - (* ltype_get_match *)
    intros j d m mm dc; split.
    admit. (* ht_ft_cond (ltype_get BlindAuction__secrets_var d) *)
    apply mm.
  - (* ltype_set_match *)
    intros f j d m fc mm dc m' disjoint_eq match_indirect.
    constructor.
    admit. (* [relate_AbData j (ltype_set ... f d) (snd m')]: premise does not
              bound [snd m'] *)
    constructor.
    split.
    + exact match_indirect.
    + admit. (* [Mem.range_perm (fst m') ...]: premise does not bound the
                permissions of [fst m'] *)
Qed.

(* Do not assume this context until now to avoid above proofs using the wrong one *)
Context`{BLINDAUCTION_pres_inv : !BLINDAUCTION_preserves_invariants}.

Lemma BlindAuction_initialize_prf : function_constr_prf BlindAuction_initialize.
Proof. solve_function_constr_prf @BlindAuction_initialize. Qed.

Theorem BlindAuction_initialize_code_correct:
  simRR GetHighDataX GetLowDataX (path_inj Hcompatrel)
    (ident_BlindAuction_initialize ↦ CompatGenSem.gensem BlindAuction_initialize_spec)
    (clight_DeepSpec_layer GetLowLayer ident_BlindAuction_initialize BlindAuction_initialize_cfun).
Proof.
  assert (cc := cl_sim_sem_code_correct).
  apply cc; clear cc; try reflexivity.
  - rewrite BlindAuction_initialize_spec_eq.
    change BlindAuction_initialize_cfun
      with (synth_func_func BlindAuction_initialize).
    apply synth_correct_2_void.
    + exact BlindAuction_initialize_prf.
    + exact BlindAuction_initialize_vc.
    + exact BlindAuction_initialize_oblg.
(*  - apply semof_prim_kernel_mode_2_void.
    + exact relate_AbData_translates_kernel_mode.
    + exact BlindAuction_initialize_spec_requires_kernel_mode. *)
Qed.

Lemma BlindAuction_reveal_prf : function_constr_prf BlindAuction_reveal.
Proof. solve_function_constr_prf @BlindAuction_reveal. Qed.

Theorem BlindAuction_reveal_code_correct:
  simRR GetHighDataX GetLowDataX (path_inj Hcompatrel)
    (ident_BlindAuction_reveal ↦ CompatGenSem.gensem BlindAuction_reveal_spec)
    (clight_DeepSpec_layer GetLowLayer ident_BlindAuction_reveal BlindAuction_reveal_cfun).
Proof.
  assert (cc := cl_sim_sem_code_correct).
  apply cc; clear cc; try reflexivity.
  - rewrite BlindAuction_reveal_spec_eq.
    change BlindAuction_reveal_cfun
      with (synth_func_func BlindAuction_reveal).
    apply synth_correct_2_void.
    + exact BlindAuction_reveal_prf.
    + exact BlindAuction_reveal_vc.
    + exact BlindAuction_reveal_oblg.
(*  - apply semof_prim_kernel_mode_2_void.
    + exact relate_AbData_translates_kernel_mode.
    + exact BlindAuction_reveal_spec_requires_kernel_mode. *)
Qed.

Lemma BlindAuction_bid_prf : function_constr_prf BlindAuction_bid.
Proof. solve_function_constr_prf @BlindAuction_bid. Qed.

Theorem BlindAuction_bid_code_correct:
  simRR GetHighDataX GetLowDataX (path_inj Hcompatrel)
    (ident_BlindAuction_bid ↦ CompatGenSem.gensem BlindAuction_bid_spec)
    (clight_DeepSpec_layer GetLowLayer ident_BlindAuction_bid BlindAuction_bid_cfun).
Proof.
  assert (cc := cl_sim_sem_code_correct).
  apply cc; clear cc; try reflexivity.
  - rewrite BlindAuction_bid_spec_eq.
    change BlindAuction_bid_cfun
      with (synth_func_func BlindAuction_bid).
    apply synth_correct_1_void.
    + exact BlindAuction_bid_prf.
    + exact BlindAuction_bid_vc.
    + exact BlindAuction_bid_oblg.
(*  - apply semof_prim_kernel_mode_1_void.
    + exact relate_AbData_translates_kernel_mode.
    + exact BlindAuction_bid_spec_requires_kernel_mode. *)
Qed.

Lemma BlindAuction_withdraw_prf : function_constr_prf BlindAuction_withdraw.
Proof. solve_function_constr_prf @BlindAuction_withdraw. Qed.

Theorem BlindAuction_withdraw_code_correct:
  simRR GetHighDataX GetLowDataX (path_inj Hcompatrel)
    (ident_BlindAuction_withdraw ↦ CompatGenSem.gensem BlindAuction_withdraw_spec)
    (clight_DeepSpec_layer GetLowLayer ident_BlindAuction_withdraw BlindAuction_withdraw_cfun).
Proof.
  assert (cc := cl_sim_sem_code_correct).
  apply cc; clear cc; try reflexivity.
  - rewrite BlindAuction_withdraw_spec_eq.
    change BlindAuction_withdraw_cfun
      with (synth_func_func BlindAuction_withdraw).
    apply synth_correct_0_void.
    + exact BlindAuction_withdraw_prf.
    + exact BlindAuction_withdraw_vc.
    + exact BlindAuction_withdraw_oblg.
(*  - apply semof_prim_kernel_mode_0_void.
    + exact relate_AbData_translates_kernel_mode.
    + exact BlindAuction_withdraw_spec_requires_kernel_mode. *)
Qed.

Lemma BlindAuction_auctionEnd_prf : function_constr_prf BlindAuction_auctionEnd.
Proof. solve_function_constr_prf @BlindAuction_auctionEnd. Qed.

Theorem BlindAuction_auctionEnd_code_correct:
  simRR GetHighDataX GetLowDataX (path_inj Hcompatrel)
    (ident_BlindAuction_auctionEnd ↦ CompatGenSem.gensem BlindAuction_auctionEnd_spec)
    (clight_DeepSpec_layer GetLowLayer ident_BlindAuction_auctionEnd BlindAuction_auctionEnd_cfun).
Proof.
  assert (cc := cl_sim_sem_code_correct).
  apply cc; clear cc; try reflexivity.
  - rewrite BlindAuction_auctionEnd_spec_eq.
    change BlindAuction_auctionEnd_cfun
      with (synth_func_func BlindAuction_auctionEnd).
    apply synth_correct_0_void.
    + exact BlindAuction_auctionEnd_prf.
    + exact BlindAuction_auctionEnd_vc.
    + exact BlindAuction_auctionEnd_oblg.
(*  - apply semof_prim_kernel_mode_0_void.
    + exact relate_AbData_translates_kernel_mode.
    + exact BlindAuction_auctionEnd_spec_requires_kernel_mode. *)
Qed.

End EdsgerGen.
