(* *********************************************************************)
(*    DeepSpec, the language of certified softwares                    *)
(*                                                                     *)
(*      Shu-Chun Weng, Yale University                                 *)
(*                                                                     *)
(*  Copyright (c) 2013-2015 Shu-Chun Weng <shu-chun.weng@yale.edu>.    *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU General Public License        *)
(*  version 2 as published by the Free Software Foundation.  Note that *)
(*  the only valid version of the GPL for this work is version 2, not  *)
(*  v2.2 or v3.x or whatever.                                          *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(*  You should have received a copy of the GNU General Public License  *)
(*  along with this program; if not, write to the Free Software        *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,         *)
(*  MA 02110-1301 USA.                                                 *)
(* *********************************************************************)

(** The run-time library for certified programs generated by [edsger] *)

(* Standard library modules *)
Require Export BinInt.

(* CompCert modules *)
Require Export backend.Cop.
Require Export backend.Ctypes.
Require Export cclib.Coqlib.
Require Export cclib.Integers.
Require Export cclib.Maps.

(* CertiKOS modules *)
(*
Require        liblayers.compat.CompatGenSem.
Require        liblayers.compcertx.MakeProgram.
Require        liblayers.compcertx.StencilImpl.
Require        mcertikos.layerlib.PrimSemantics.
 *)

(* DeepSpec modules *)
Require Export DeepSpec.core.Cval.
Require Export DeepSpec.core.SEnv.
Require Export DeepSpec.core.HyperType.
Require Export DeepSpec.core.HyperTypeInst.
Require Export DeepSpec.core.MemoryModel.
Require Export DeepSpec.core.Syntax.
Require Export DeepSpec.core.SynthesisFunc.
Require Export DeepSpec.core.SynthesisStmt.
Require Export DeepSpec.lib.OProp.
(* Todo: do we need this export? *)
Require Export DeepSpec.lib.IndexedMaps.
Require Export DeepSpec.lib.SimpleMaps.
Require Export DeepSpec.lib.SimpleIndexedMaps.

(* We don't import liblayers.logic.Structures, which populates the notation
   scope too much; instead we declare the minimum we need. *)
(*
Infix "⊕" := liblayers.logic.Structures.oplus (at level 60, right associativity).
Infix "↦" := liblayers.logic.Structures.mapsto (at level 55, right associativity).
*)
Notation tchar := (Tint I8 Unsigned).

(* This file will contain definitions related to each type in DeepSpec,
   defining the corresponding lenses. *)

(* Default value for global pointers.

   The current version of DeepSpec does not provide pointers as a primitive type, but
   if we added an address-of operator, then this would be used.  *)
(*
Definition empty_globalpointer := GLOBUndef.
 *)

Lemma and_iff_compat {A B C D} : (A <-> C) -> (B <-> D) -> ((A /\ B) <-> (C /\ D)).
Proof.
  intros A_iff_C B_iff_D.
  rewrite A_iff_C, B_iff_D.
  apply iff_refl.
Qed.

Ltac solve_algebraic_record_data_type_hyper_type hyper_type_impl :=
 esplit;
  [ intros; eexists; reflexivity
  | unfold ht_ft_cond, ht_default, hyper_type_impl;
    repeat match goal with
    | |- _ /\ _ => split
    | |- True => exact I
    end; apply ht_default_ft_cond
  | intros f; apply iff_refl || (
    unfold ht_valid_ft_cond, ht_valid_ft_ocond, hyper_type_impl;
    rewrite ? oand1_distr, ? OProp1map1; [| exact I .. ];
    repeat (apply and_iff_compat; try apply @ht_valid_ft_ocond_same);
      typeclasses eauto)
  ].

Ltac solve_record_type_hyper_field :=
   constructor;
     [ reflexivity   (* size_pos *)
     | simpl; tauto  (* get_returns *)
     | simpl; tauto  (* set_returns *)
     | simpl; intros; constructor; reflexivity   (* get_correct*) 
     | intros part whole pcond wcond; constructor; reflexivity (* set_correct *)
     | eexists; eexists;
       repeat apply conj; reflexivity  (* delta_correct *)
     ].

(* Previous version. Still unclear on this... *)
(* Ltac solve_record_type_hyper_field :=
   esplit;
     [ reflexivity
     | simpl; auto  
     | intros whole wcond; apply wcond
     | intros part whole pcond wcond; simpl; repeat apply conj; solve [ apply pcond | apply wcond ]
     | constructor; reflexivity
     | intros part whole pc wc; apply ht_some_cval; reflexivity
     | eexists; eexists;
       repeat apply conj; reflexivity
     ]. *)

Hint Extern 1 ((_ < _)%Z) => reflexivity : zarith.
Hint Extern 1 ((_ > _)%Z) => reflexivity : zarith.

(* TODO: Oops, the currently admitted condition is supposed to be available from the context, we need to track down where it's supposed to come from. *)
Ltac solve_twobranch_type_hyper_field :=
  esplit;
    [  simpl; constructor (* size_pos *)
    | intros whole wcond;
      try destruct whole as [ | ]; simpl in *; try tauto; auto with zarith; apply wcond (* get_returns *)
    | intros part whole pcond wcond; simpl; try destruct whole as [ | ]; repeat apply conj; solve [ apply pcond | apply wcond ] (* set_returns *)
    | intros whole;
      try destruct whole as [ | ]; simpl in *; try tauto;
      constructor; reflexivity  (* get_correct *)
    | simpl; intros part whole;
      assert (wvalid : ht_valid_ft_cond whole) by admit;
      try destruct whole as [ | ]; simpl in wvalid; try tauto;
      constructor; reflexivity (* set_correct *)
    | eexists; eexists;
      repeat apply conj; reflexivity  (* delta_correct *)
    ].

Definition array_cval {tp} `{HyperTypeImpl tp} arr (size : Z) :=
  CVarray (*unpair_ty tp*) (CAmap (ZMap.map (@ht_cval tp _) arr)).

Lemma forall_suchthat_iff_compat {T P A B} :
  (forall t : T, P t -> (A t <-> B t)) ->
  ((forall t : T, P t -> A t) <-> (forall t : T, P t -> B t)).
Proof.
  intros IH; split; intros other t p;
    [ rewrite <- IH | rewrite IH ]; try apply other; exact p.
Qed.

Lemma forall_suchthat_iff_compat2 {T S P A B} :
  (forall (t : T) (s:S), P t s -> (A t s <-> B t s)) ->
  ((forall (t : T) (s:S), P t s -> A t s) <-> (forall (t : T) (s:S), P t s -> B t s)).
Proof.
  intros IH; split; intros other t s p;
    [ rewrite <- IH | rewrite IH ]; try apply other; exact p.
Qed.


Open Scope Z_scope.

Lemma zle_and_zlt_between a b c : a <= b < c -> zle a b && zlt b c = true.
Proof.
  intros [ a_le_b b_lt_c ].
  destruct (zle a b); try contradiction.
  destruct (zlt b c); try contradiction.
  reflexivity.
Qed.

(* Lenses to do with arrays. *)
Section ZMAP_BASED_ARRAY.
  Variable tp : type_pair.
  (* Hypothesis tp_naturally_aligned : naturally_aligned (unpair_ty tp). *)
  Context`{hti : HyperTypeImpl tp}.
  Variable sz : Z.

  Let arr_tp := Tpair (ZMap.t (unpair_ft tp)) (Tarray (unpair_ty tp) sz).

  Local Instance ZMap_based_array_hyper_type_impl : HyperTypeImpl arr_tp := {
    ht_cval f := array_cval (*tp*) f sz;
    ht_ft_cond f := forall n, 0 <= n < sz -> @ht_ft_cond tp _ (ZMap.get n f);
    ht_default := ZMap.init (@ht_default tp _);
    ht_valid_ft_cond f := forall n, 0 <= n < sz ->
      @ht_valid_ft_cond tp _ (ZMap.get n f);
    ht_valid_ft_ocond :=
      omap1 (fun p f => forall n, 0 <= n < sz -> p (ZMap.get n f))
            (@ht_valid_ft_ocond tp _);
(*    ht_inject j a b := forall n, (*0 <= n < sz ->*) (* strengthened for passthrough *)
      @ht_inject tp _ j (ZMap.get n a) (ZMap.get n b) *)
  }.

  Local Instance ZMap_based_array_hyper_type`{ht : !HyperType tp} : HyperType arr_tp.
  Proof. esplit.
    - intros; eexists; reflexivity.
    - intros n n_range.
      unfold ht_default, ZMap_based_array_hyper_type_impl.
      rewrite ZMap.gi.
      apply ht_default_ft_cond.
    - intros f.
      unfold ht_valid_ft_cond, ht_valid_ft_ocond, ZMap_based_array_hyper_type_impl.
      change (ZMap.t (unpair_ft tp)) with (unpair_ft arr_tp).
      rewrite OProp1map1; [| intros; exact I ].
      apply forall_suchthat_iff_compat.
      intros; apply ht_valid_ft_ocond_same.
  Qed.

  Local Instance ZMap_based_array_hyper_index_impl : HyperIndexImpl arr_tp tp := {
    Hindex_size := sz;
    Hindex_get idx ar := ZMap.get idx ar;
    Hindex_set idx elem ar := ZMap.set idx elem ar
  }.

  Local Instance ZMap_based_array_hyper_index`{ht : !HyperType tp} : HyperIndex arr_tp tp.
  Proof. Admitted.
(*   Proof. esplit. *)
(*     - (* Hindex_get_returns *) *)
(*       intros a i i_range ac; apply ac, i_range. *)
(*     - (* Hindex_set_returns *) *)
(*       simpl; intros f a i i_range fc ac i' i'_range. *)
(*       destruct (zeq i' i) as [ -> | i_ne ]; *)
(*         [ rewrite ? ZMap.gss; apply fc *)
(*         | rewrite ? ZMap.gso; [ apply ac, i'_range | apply i_ne ] *)
(*         ]. *)
(*     - (* Hindex_get_correct *) *)
(*       simpl; intros a i i_range. *)
(*       rewrite zle_and_zlt_between, ZMap.gmap; try assumption. *)
(*       constructor. *)
(*       reflexivity. *)
(*     - (* Hindex_set_correct *) *)
(*       simpl; intros f a i i_range. *)
(*       rewrite zle_and_zlt_between; try assumption. *)
(*       constructor. *)
(*       simpl; unfold array_cval. *)
(*       do 2 f_equal. *)
(*       apply eq_sym, ZMapAux.smap. *)
(*     - (* Hindex_array_type *) *)
(*       eexists; reflexivity. *)
(* (*    - (* Hindex_naturally_aligned *) *)
(*       exact tp_naturally_aligned. *) *)
(*   Qed. *)

  Local Instance ZMap_based_array_hyper_index_passthrough :
      HyperIndexPassthrough arr_tp tp.
(*  Proof. esplit; simpl.
    - intros j a b.
      exact (fun x => x).
    - intros j elem_a arr_a elem_b arr_b elem_inj arr_inj i n.
      destruct (zeq n i) as [ -> | i_ne ].
      + rewrite 2 ZMap.gss.
        exact elem_inj.
      + rewrite 2 ZMap.gso; try assumption.
        apply arr_inj.
  Qed. *)
End ZMAP_BASED_ARRAY.


Definition hashmap_cval {tp} `{HyperTypeImpl tp} arr :=
  CVhashmap
    (CHmap (map_of_tree ht_cval ht_default arr)).

(* Lenses to do with hashmaps. *)
Section INT256TREE_BASED_INT_HASHMAP.
  Variable tp : type_pair.
  (* Hypothesis tp_naturally_aligned : naturally_aligned (unpair_ty tp). *)
  Context`{hti : HyperTypeImpl tp}.
  
  Let arr_tp := Tpair (Int256Tree.t (unpair_ft tp)) (Thashmap tint (unpair_ty tp)).

  Local Instance Int256Tree_based_hashmap_hyper_type_impl : HyperTypeImpl arr_tp := {
    ht_cval f := hashmap_cval (*tp*) f;
    ht_ft_cond f := forall n v, Int256Tree.get n f = Some v ->  @ht_ft_cond tp _ v;
    ht_default := (Int256Tree.empty (unpair_ft tp));
    ht_valid_ft_cond f := forall n v, Int256Tree.get n f = Some v ->
      @ht_valid_ft_cond tp _ v;
    ht_valid_ft_ocond := 
      omap1 (fun p f => forall n v, Int256Tree.get n f = Some v -> p v)
            (@ht_valid_ft_ocond tp _);
  }.

  Local Instance Int256Tree_based_hashmap_hyper_type`{ht : !HyperType tp} : HyperType arr_tp.
  Proof. esplit.
    - intros; eexists; reflexivity.
    - intros n.
      unfold ht_default, Int256Tree_based_hashmap_hyper_type_impl.
      rewrite Int256Tree.gempty.
      intros; congruence.
    - intros f.
      unfold ht_valid_ft_cond, ht_valid_ft_ocond, Int256Tree_based_hashmap_hyper_type_impl.
      change (Int256Tree.t (unpair_ft tp)) with (unpair_ft arr_tp).
      rewrite OProp1map1; [| intros; exact I ].
      apply forall_suchthat_iff_compat2.
      intros; apply ht_valid_ft_ocond_same.
  Qed.

  Local Instance Int256Tree_based_hashmap_hash_impl : HyperIntHashImpl arr_tp tp := {
    Hhash_get idx ar := Int256Tree.get_default ht_default idx ar;
    Hhash_set idx elem ar := Int256Tree.set idx elem ar
  }.

  Require Import lib.ArithInv.

  Local Instance Int256Tree_based_hashmap_hash `{ht : !HyperType tp} : HyperIntHash arr_tp tp.
  Proof. esplit.
    - (* Hhash_get_returns *)
      intros a i ac.
      simpl.
      unfold Int256Tree.get_default.
      destruct (Int256Tree.get i a) eqn:?.
      + apply (ac _ _ Heqo).
      + apply ht_default_ft_cond.
    - (* Hhash_set_returns *)
      simpl; intros f a i fc ac i'.
      intros.
      destruct (Int256.eq i' i) eqn:i_eq;  inv_arith.
      + subst.
        rewrite Int256Tree.gss in H.
        inversion H.
        subst; auto.
      + rewrite Int256Tree.gso in H by assumption.
        eapply ac; eauto.
    - (* Hhash_get_correct *)
      simpl; intros a i.
      unfold Int256Tree.get_default.
      destruct (Int256Tree.get i a) as [v|] eqn:?.
      + eapply (map_of_tree_Some ht_cval ht_default)  in Heqo.
        rewrite Heqo.
        constructor.
        reflexivity.
      + apply (map_of_tree_None ht_cval ht_default) in Heqo.
        rewrite Heqo.
        constructor.
        reflexivity.
    - (* Hhash_set_correct *)
      simpl; intros f a i.
      constructor.
      simpl.
      unfold hashmap_cval.
      rewrite map_of_tree_set.
      reflexivity.
    - (* Hhash_array_type *)
      eexists; reflexivity.
  Qed.
  
End INT256TREE_BASED_INT_HASHMAP.  


(* Recall that layer interfaces are specified in terms of a Coq relations (the "semantics"),
   while the desugaring uses Coq functions.

   These functions convert desugared values into the semantic values that the layer interface specifications expect.
 *)

(* Omitting all of this for now... *)
(*
Section SEMOF_HYPER_TYPE.
  Context {mem}`{Hmem: Mem.MemoryModel mem}`{Hmwd: MemWithData.UseMemWithData mem}.
  Context {data : Type}.
  Context`{LayerSpec : !LayerSpecClass}.
  Context {ft ty}`{ht : HyperType (Tpair ft ty), hta : !HyperArgRet (Tpair ft ty)}.

  Let unfold_cval cv := match cv with
    | CVval v => v
    | _ => Vundef
    end.

  (* Roughly, this typeclass instance says how to give a relational semantics for desugared functions of type
       (data -> option (data * ft))
     in terms of a layerlib relation (which involves zero arguments, a return type ty, and implicitly some state d).
   *)
  Inductive semof_nil_ft {data}
      : GenSem.Semof data (data -> option (data * ft)) Tnil ty :=
  | semof_nil_ft_intro f (d : data) d' a v:
      f d = Monad.ret (d', a) ->
      ht_ft_cond a -> ht_valid_ft_cond a ->
      unfold_cval (ht_cval a) = v ->
      GenSem.semof f nil d v d'.

  Global Existing Instance semof_nil_ft.

  (* Each semantic needs to satisfy some extra properties, so we prove those. *)
  Global Instance semof_nil_ft_props
      : GenSem.Semprops (data -> option (data * ft)).
  Proof.
    split.
    + (* semprops_well_typed *)
      intros ? ? ? ? ? H.
      inv H.
      assert (ht_has_type := ht_has_type).
      eapply ht_has_type; try eassumption.
      unfold ht_rel.
      exploit ht_basic; try eassumption; inversion 1;
        constructor; reflexivity.
    + (* semprops_arity *)
      intros ? ? ? ? ? H.
      inv H;
      reflexivity.
    + (* semprops_lessdef *)
      intros ? ? ? ? ? ? H Hl.
      inv H;
      inv Hl;
      reflexivity.
    + (* semprops_inject_neutral *)
      inversion 1; subst.
      exploit ht_basic; try eassumption; inversion 1; constructor.
    + (* semprops_determ *)
      inversion 1.
      inversion 1.
      unfold Monad.bind, Monad.ret in *; simpl in *.
      split; [ assert (a0 = a) |]; try congruence.
    + (* semprops_inject *)
      inversion 1.
      inversion 1.
      reflexivity.
  Qed.

  (* We use the above definition to give semantics to pure functions (which do not change the abstract data). *)
  Local Notation lift_nil_ft_pure f :=
    (fun d => Monad.bind (f d) (fun b => Monad.ret (d, b))).

  Global Instance semof_nil_ft_pure
      : GenSem.Semof data (data -> option ft) Tnil ty :=
    fun f => GenSem.semof (lift_nil_ft_pure f).

  Global Instance semof_nil_ft_pure_props
      : GenSem.Semprops (data -> option ft).
  Proof.
    split; intro f.
    + exact (GenSem.semprops_well_typed (lift_nil_ft_pure f)).
    + exact (GenSem.semprops_arity (lift_nil_ft_pure f)).
    + exact (GenSem.semprops_lessdef (lift_nil_ft_pure f)).
    + exact (GenSem.semprops_inject_neutral (lift_nil_ft_pure f)).
    + exact (GenSem.semprops_determ (lift_nil_ft_pure f)).
    + exact (GenSem.semprops_inject (lift_nil_ft_pure f)).
  Qed.

  Global Instance semof_nil_ft_pure_invar `{CompatData data}(f: data -> option ft):
    CompatGenSem.PreservesInvariants f.
  Proof.
    split.
    * intros ? ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? spec ]; subst.
      unfold Monad.bind, Monad.ret in spec; simpl in spec.
      destruct (f d); try discriminate spec.
      injection spec as _ <-; assumption.
    * intros ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? spec ]; subst.
      unfold Monad.bind, Monad.ret in spec; simpl in spec.
      destruct (f d); try discriminate spec.
      injection spec as _ <-; assumption.
    * intros ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? spec ]; subst.
      unfold Monad.bind, Monad.ret in spec; simpl in spec.
      destruct (f d); try discriminate spec.
      injection spec as _ <-; assumption.
  Qed.

  (* Then we give semantics for functions that take more than zero arguments.
     We use some "generic programming" to specify this in terms of prepending one argument at a time.
   *)
  Inductive semof_cons_ft `{GenSem.Semof data}
    : GenSem.Semof data (ft -> T) (Tcons ty targs) tres :=
  | semof_cons_ft_intro f a l d v d' va:
    GenSem.semof (f a) l d v d' ->
    ht_ft_cond a -> ht_valid_ft_cond a ->
    unfold_cval (ht_cval a) = va ->
    GenSem.semof f (va :: l) d v d'.

  Global Existing Instance semof_cons_ft.

  Global Instance semof_cons_ft_props {T}`(HT: GenSem.Semprops data T)
      : GenSem.Semprops (ft -> T).
  Proof.
    split.
    + (* semprops_well_typed *)
      intros ? ? ? ? ? H.
      inv H.
      apply (GenSem.semprops_well_typed (targs := targs) (f a) l d vres d').
      eassumption.
    + (* semprops_arity *)
      intros ? ? ? ? ? H.
      inv H.
      simpl.
      f_equal.
      eapply GenSem.semprops_arity.
      eassumption.
    + (* semprops_lessdef *)
      intros until d'.
      intros H Hl.
      inv H.
      inv Hl.
      inv H4.
      * f_equal.
        eapply GenSem.semprops_lessdef; eassumption.
      * revert H3.
        exploit ht_basic; try eassumption; inversion 1; discriminate.
    + (* semprops_inject_neutral *)
      inversion 1; subst.
      eapply GenSem.semprops_inject_neutral.
      eassumption.
    + (* semprops_determ *)
      inversion 1.
      inversion 1.
      assert (a0 = a).
      { apply ht_injective; try assumption.
        revert H3 H19.
        exploit (ht_basic a0); try eassumption.
        exploit (ht_basic a); try eassumption.
        inversion 1; inversion 1; simpl; congruence.
      }
      subst.
      eapply GenSem.semprops_determ; eassumption.
    + (* semprops_inject *)
      intros.
      inv H.
      inv H0.
      (*inv H4; try (destruct b; discriminate).*)
      f_equal.
      - revert H5.
        exploit ht_basic; try eassumption; inversion 1; inversion 1; reflexivity.
      - eapply GenSem.semprops_inject; eassumption.
  Qed.

  Global Instance semof_cons_ft_invar
      `{Tsemof: GenSem.Semof data, CompatData data} (f: ft -> T):
    (forall n, CompatGenSem.PreservesInvariants (f n)) ->
    CompatGenSem.PreservesInvariants f.
  Proof.
    intros Hf.
    split.
    * intros ? ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? ? ? semof' ]; subst.
      eapply CompatGenSem.semprops_low_level_invariant; eassumption.
    * intros ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? ? ? semof' ]; subst.
      eapply CompatGenSem.semprops_high_level_invariant; eassumption.
    * intros ? ? ? ? semof inv.
      simpl in semof.
      inversion semof as [ ? ? ? ? ? ? ? semof' ]; subst.
      eapply CompatGenSem.semprops_kernel_mode; eassumption.
  Qed.
End SEMOF_HYPER_TYPE.

Remove Hints GenSem.semof_nil_int64_pure_total
             GenSem.semof_nil_int64_pure
             GenSem.semof_nil_int64
             GenSem.semof_nil_int_pure_total
             GenSem.semof_nil_int_pure
             GenSem.semof_nil_int
             GenSem.semof_cons64
             GenSem.semof_cons : typeclass_instances.

(* These two tactics convert between layerlib semantics and the DeepSpec desugared
   representation.  *)
Ltac inv_semof' sem_of :=
  repeat
    match type of sem_of with
    | GenSem.semof _ _ _ _ _ => unfold GenSem.semof in sem_of
    | semof_nil_ft_pure _ _ _ _ _ => unfold semof_nil_ft_pure in sem_of
    | semof_cons_ft _ _ _ _ _ =>
      let a       := fresh "arg" in
      let sem_of' := fresh "sem_of'" in
      let ac      := fresh "argc" in
      let a_valid := fresh "arg_valid" in
      let a_eq    := fresh "arg_eq" in
      inversion sem_of as [ ? a ? ? ? ? ? sem_of' ac a_valid a_eq ]; subst;
      clear sem_of; rename sem_of' into sem_of
    | semof_nil_ft _ _ _ _ _ =>
      let r       := fresh "ret" in
      let sem_of' := fresh "sem_of'" in
      let rc      := fresh "retc" in
      let r_valid := fresh "ret_valid" in
      let r_eq    := fresh "ret_eq" in
      inversion sem_of as [ ? ? ? r ? sem_of' rc r_valid r_eq ]; subst;
      clear sem_of; rename sem_of' into sem_of
    | GenSem.semof_nil_void _ _ _ _ _ =>
      let sem_of' := fresh "sem_of'" in
      inversion sem_of as [ ? ? ? sem_of' ]; subst;
      clear sem_of; rename sem_of' into sem_of
    | Monad.bind ?f ?m = Monad.ret ?b =>
      let sem_of' := fresh "sem_of'" in
      let H       := fresh "H" in
      destruct (Monad.monad_inv_bind f m b sem_of) as (? & sem_of' & H);
      injection H as -> <-; clear sem_of H; rename sem_of' into sem_of
    end.

Ltac inv_semof sem_of :=
  inv_semof' sem_of;
  simpl in sem_of;
  try (match type of sem_of with
    match ?X with _ => _ end = Some _  =>
    let Hsem_of := fresh sem_of in
    destruct X eqn:Hsem_of;
    [ let H'sem_of := fresh Hsem_of in
      rename Hsem_of into H'sem_of; rename sem_of into Hsem_of; rename H'sem_of into sem_of
      | discriminate]
  end).

Ltac construct_semof :=
  repeat match goal with
  | |- GenSem.semof _ _ _ _ _ => unfold GenSem.semof
  | given : ?f _ = Monad.ret ?r |- semof_nil_ft_pure ?spec _ _ _ _ =>
    match spec with
    | context[f] => apply semof_nil_ft_intro with r; try assumption; try reflexivity
    end
  | given : appcontext[?f ?arg] |- semof_cons_ft ?f _ _ _ _ =>
    apply semof_cons_ft_intro with arg; try assumption; try reflexivity
  | given : ?f ?d = Some (_, ?r) |- semof_nil_ft ?f _ ?d _ _ =>
    apply semof_nil_ft_intro with r; try assumption; try reflexivity
  | |- GenSem.semof_nil_void _ _ _ _ _ => constructor
  | |- _ = Monad.ret _ => eassumption
  end.

(* Then we construct the liblayer semantics for primitives.
   (This is the right hand side of each entry in a layer interface.)

   There already is some code in liblayer for building primitive semantics,
   but it does it in one step directly from Coq functions. Here we do it in
   two steps,
     coq function -> c semantics -> arbitrary primitive semantics.
   This is the first step, the second step is basically just inl(-).
*)
Section primitive_primsem.
  Context {mem}`{memory_model : Mem.MemoryModel mem}.
  Context`{Hmwd : !MemWithData.UseMemWithData mem}.
  Context {D}`{HD : CompatData D}.

  Definition gencsem `{Tsemof : GenSem.Semof (cdata D)}`{Hsemof : !GenSem.Semprops T}
      (f : T){H : CompatGenSem.PreservesInvariants f} := {|
    CompatCPrimitives.sextcall_primsem_step := CompatGenSem.sextcall_generic_info f;
    CompatCPrimitives.sextcall_props := Errors.OK (CompatGenSem.extcall_generic_properties f);
    CompatCPrimitives.sextcall_invs := Errors.OK (CompatGenSem.extcall_generic_invariants f H)
  |}.

  Definition dummy_sextcall_primsem : CompatCPrimitives.sextcall_primsem (cdata D) := {|
    CompatCPrimitives.sextcall_primsem_step := {|
      CompatCPrimitives.sextcall_step WB vargs m vres m' := True;
      CompatCPrimitives.sextcall_csig := {|
        CompcertStructures.csig_args := Ctypes.Tnil;
        CompcertStructures.csig_res := Ctypes.Tvoid;
        CompcertStructures.csig_cc := AST.cc_default
      |}
    |};
    CompatCPrimitives.sextcall_props := Errors.Error nil;
    CompatCPrimitives.sextcall_invs := Errors.Error nil
  |}.
End primitive_primsem.
*)

(* Each DeepSpec project will generate one instance of this class.

  Then the Coq typeclass search will produce an instance of LayerSpecClass,
  which is used by most of synthesis functions.
 *)

Definition block := positive. (* TODO: make a better memory model. *)
Class GlobalAbData {AbData : Type}(AbData_empty_data : AbData)
    (AbData_low_level_invariant : block -> AbData -> Prop) := {
  AbData_low_level_invariant_incr : forall n n' d,
    (n <= n')%positive ->
    AbData_low_level_invariant n d ->
    AbData_low_level_invariant n' d;
  AbData_empty_data_low_level_invariant : forall n,
    AbData_low_level_invariant n AbData_empty_data
}.

(* This is an empty layer with no objects and no import/exports.
   The lowest layer defined by the user will build on top of this. *)
Section BUILTIN_LAYER.
  Context`{LayerSpec : LayerSpecClass}.
  Context {AbData_empty_data : GetHighData}.
  Context {AbData_low_level_invariant : block -> GetHighData -> Prop}.
  Context`{global_abdata : !GlobalAbData AbData_empty_data AbData_low_level_invariant}.

  Instance BuiltinBase_data_ops : CompatDataOps GetHighData := {
    empty_data := AbData_empty_data;
    high_level_invariant d := True;
    (*low_level_invariant := AbData_low_level_invariant; 
    kernel_mode d := True *)
  }.
  
  Instance BuiltinBase_data : CompatData GetHighData  := {
(*    low_level_invariant_incr := AbData_low_level_invariant_incr;
    empty_data_low_level_invariant := AbData_empty_data_low_level_invariant; *)
    empty_data_high_level_invariant := I
  }.

  Definition BuiltinBase_cdata : compatdata := cdata GetHighData.

  (*
  Definition BuiltinBase_Layer : liblayers.compat.CompatLayerDef.compatlayer BuiltinBase_cdata
    := Structures.emptyset. *)
End BUILTIN_LAYER.

(* We start numbering identifiers from 10 instead of 1.

   This is because CompCert uses some low-numbered idenfiers for special variables, and
   when printing error messages it will print hardcoded names for these instead of numbers(!),
   which would be very confusing.
*)
Notation BuiltinBase_local_ident_start := 10%positive.

(* Tactics to solve the _wellformed and _prf conditions.
   Edsger will call these in the generated LayerFoo.v files.
*)
Ltac solve_wellformed :=
  compute -[Int256.repr Int256.modulus not];
  repeat match goal with
  | |- prod ?X ?Y => split
  | |- SimpleIndexedMaps.Isomorphism ?X ?Y =>
       (* simpl may still leave some symbols folded, definitional
          equivalent terms may not be syntactically identical. *)
    apply SimpleIndexedMaps.identity_automorphism
  | |- True => exact I
  | |- ?X <> ?Y => discriminate
  end;
  match goal with
  | |- ?g => idtac "unknown well-formed condition" g
  end.

(*
Ltac solve_passthrough_prf f :=
  unfold f;
  repeat match goal with
    | |- variable_passthrough_prf _ => assumption
    | |- expr_constr_passthrough_prf_conj _ =>
         unfold expr_constr_passthrough_prf_conj; simpl
    | |- { _ : _ | _ } => econstructor
    | _ => typeclasses eauto
    | |- cmd_constr_passthrough_prf _ (CCrespec_opt _ _ _) =>
      constructor; intros; CommonTactic.subdestruct; eexists; intros; reflexivity
    (*
    | |- function_constr_passthrough_prf _ => constructor
    | |- cmd_constr_passthrough_prf _ _ => constructor
    | |- lexpr_constr_passthrough_prf _ => constructor
    | |- expr_constr_passthrough_prf _ => constructor
    *)
    | _ => constructor
    end.
 *)

Ltac solve_function_constr_prf f :=
  unfold f;
  esplit; [ simpl; typeclasses eauto (* HyperArgRet *) || exact I |];
  repeat match goal with
  | |- variable_prf _ => constructor; [ reflexivity | typeclasses eauto ]
  | |- expr_constr_prf_conj _ => unfold expr_constr_prf_conj; simpl
  | |- @ht_ft_cond _ int_Z32_impl _ => split; reflexivity
  | |- @ht_ft_cond _ int_bool_impl _ => simpl
  (*
  | |- primitive_prf _ => typeclasses eauto
  | |- primitive_passthrough_prf _ => typeclasses eauto
  | |- HyperField _ _ _ => typeclasses eauto
  | |- HyperIndex _ _ => typeclasses eauto
  | |- HyperByValueType _ _ => typeclasses eauto
  | |- HyperBinaryOp _ _ _ _ => typeclasses eauto
  *)
  | |- _ => typeclasses eauto
  (*
  | |- cmd_constr_prf _ _ _ _ => econstructor
  | |- lexpr_constr_prf _ => econstructor
  | |- expr_constr_prf _ => econstructor
  | |- True => constructor
  | |- _ = _ => constructor
  | |- _ /\ _ => constructor
  | |- { _ : _ | _ } => econstructor
  *)
  | |- _ => econstructor
  end.

(* "safety condition destruct". This unfolds synth_stmt_spec_cond. *)
Ltac scdestruct sc :=
  match type of sc with
  | True => clear sc
  | ?X = ?X => clear sc
  | _ /\ _ =>
    let sc1 := fresh "sc1" in
    let sc2 := fresh "sc2" in
    destruct sc as [ sc1 sc2 ];
    scdestruct sc1; scdestruct sc2
  | match ?X with _ => _ end = Some _ =>
    let sc1 := fresh "sc" in
    destruct X eqn:sc1;
    try discriminate sc;
    scdestruct sc1 (* ; scdestruct sc *)
  | _ => progress hnf in sc; scdestruct sc
  | _ => idtac
  end.

(* These unfold just enough definitions so that the verification conditions can be discharged. *)
Ltac vcgen_static eval_in :=
  match goal with
  | |- _ /\ _ => split; vcgen_static eval_in
  | |- True => exact I
  | |- forall X, @?P X -> _ =>
    let X' := fresh X in
    let P' := fresh X' "_cond" in
    intros X' P';
    eval_in P';
    vcgen_static eval_in
  | |- _ -> False => idtac
  | |- _ -> _ =>
    let sc := fresh "sc" in
    intros sc;
    eval_in sc;
    scdestruct sc; vcgen_static eval_in
  | |- let X := _ in _ =>
    let X' := fresh X in
    intros X';
    eval_in X';
    vcgen_static eval_in
  | _ => idtac
  end.
(* Ltac vcgen' eval_in eval_goal steps := *)
(*   vcgen_static eval_in; *)
(*   match goal with *)
(*   | |- Z.lt _ _ => eval_goal *)
(*   | |- Z.gt _ _ => eval_goal *)
(*   | |- Z.le _ _ => eval_goal *)
(*   | |- Z.ge _ _ => eval_goal *)
(*   | |- not _ => eval_goal *)
(*   | |- synth_stmt_cond ?c _ _ _ _ _ => *)
(*     match c with *)
(*     | CCskip => idtac *)
(*     | CCload _ => idtac *)
(*     | CCyield _ => idtac *)
(*     | CCpanic => idtac *)
(*     end; *)
(*     unfold synth_stmt_cond; eval_goal *)
(*   | _ => match steps with *)
(*     | S ?steps' => progress hnf; vcgen' eval_in eval_goal steps' *)
(*     | _ => idtac *)
(*     end *)
(*   end. *)

(* Ltac cbv_minus := cbv -[Int256.repr Int256.modulus zeq zle zlt Z.iter Z.le Z.lt Z.gt Z.ge Z.eqb Z.leb Z.ltb Z.geb Z.gtb Z.mul Z.div Z.modulo Z.add Z.sub Z.shiftl Z.shiftr Z.lxor Z.land Z.lor *)
(*                               is_true bool_dec  ZMap.get ZMap.set hlist_hd *)
(*                               omap2 oand2 oimply2 oabsorption2 hlist_param_func SpecTree.get SpecTree.get_eq]. *)

Lemma repr_Z_mod_modulus_eq: forall x, Int.repr (Int.Z_mod_modulus x) = Int.repr x.
Proof.
  intro x.
  Transparent Int.repr.
  unfold Int.repr.
  Local Opaque Int.repr.
  apply Int.mkint_eq.
  rewrite Int.Z_mod_modulus_eq.
  apply Zmod_small.
  apply Int.Z_mod_modulus_range.
Qed.

(* Related to reentrancy tracking using the Checks Effects Interactions Pattern: *)

(* CEI_auto tries to solve a goal of the form 
   cmd_constr_CEI_pattern_prf ____ rst_before _____ rst_after
*)
Ltac CEI_auto :=
  repeat (
  reflexivity
+ typeclasses eauto 
+ eapply CCCEIPskip
+ eapply CCCEIPlet 
+ eapply CCCEIPload  
+ eapply CCCEIPstore 
+ eapply CCCEIPsequence 
+ eapply CCCEIPifthenelse1 
+ eapply CCCEIPifthenelse2 
+ eapply CCCEIPifthenelse3 
+ eapply CCCEIPifthenelse4 
+ eapply CCCEIPifthenelse5 
+ eapply CCCEIPfor 
+ eapply CCCEIPfirst 
+ eapply CCCEIPfold 
+ eapply CCCEIPcall1 
+ eapply CCCEIPcall2 
+ eapply CCCEIPyield 
+ eapply CCCEIPconstr 
+ eapply CCCEIPtransfer 
+ eapply CCCEIPassert 
+ eapply CCCEIPdeny 
+ eapply CCCEIPpanic 
+ eapply CCCEIPrespec 
+ eapply CCCEIPrespec_opt)
  .

(* CEI_auto_states_A tries to solve a CEI goal by trying the pairs of states shown below. *)
Ltac CEI_auto_states_A := 
  solve [
      (simpl; exists (Safe_no_reentrancy, Safe_no_reentrancy); CEI_auto)
    | (simpl; exists (Safe_no_reentrancy, Safe_with_potential_reentrancy); CEI_auto)
  ].

(* CEI_auto_states_B tries to solve a CEI goal by trying the pairs of states shown below. *)
Ltac CEI_auto_states_B := 
  solve [
      (simpl; exists (Safe_with_potential_reentrancy, Safe_with_potential_reentrancy); CEI_auto)
  ].

(* CEI_auto_AB tries to solve a CEI goal by first trying the pairs of states from CEI_auto_states_A
     then CEI_auto_states_B. *)
Ltac CEI_auto_AB :=
  solve [CEI_auto_states_A | CEI_auto_states_B].

(* CEI_auto_BA tries to solve a CEI goal by first trying the pairs of states from CEI_auto_states_B
     then CEI_auto_states_A. *)
Ltac CEI_auto_BA :=
  solve [CEI_auto_states_B | CEI_auto_states_A].

(* CEI_auto_AB and BA are intended to be used to generate the PRIMrst_before/after_A/B entries in 
  _prim entries, later used in higher level cmd_constr_CEI_pattern_prf goals. *)

Inductive External_contract_call_argument :=
  | External_contract_call_int_argument (n : Z)
  | External_contract_call_bool_argument (bool : Z)
.

Inductive External_call_info :=
  | External_transfer (recipient amount : Z)
  | External_contract_call (external_contract_address : Z) (args : list External_contract_call_argument)
.

Inductive External_action_info_type :=
  | NoExternalAction
  | SomeExternalActionAndFollowingCEIP (c : External_call_info)
  | ErrorNotFolllowingCEIP
.