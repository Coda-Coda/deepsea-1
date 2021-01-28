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

(** Synthesizing Clight functions and specifications from DeepSpec along
    with the proofs.  *)

(* Standard library modules *)
Require Import BinPos.
Require Import List.

(* CompCert modules *)
Require Import backend.AST.
(*Require Import compcert.common.Events.
Require Import compcert.common.Globalenvs. *)
Require Import backend.Values.HighValues.
Require Import backend.MemoryModel.
Require Import cclib.Coqlib.
Require Import cclib.Maps.
Require Import backend.Ctypes.
Require Import backend.Expressions.ExpMiniC.
Require Import backend.Statements.StmtMiniC.
Require Import backend.MachineModel.

(* CompCertX modules *)
(* Require Import compcertx.cfrontend.ClightBigstepX. *)

(* DeepSpec modules *)
Require Import DeepSpec.lib.IndexedMaps.
Require Import DeepSpec.lib.SimpleMaps.
Require Import DeepSpec.lib.SimpleIndexedMaps.
Require Import DeepSpec.lib.OProp.
Require        DeepSpec.lib.LangDef.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.HyperType.
(*Require Import DeepSpec.core.HyperMem.*)
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.core.Syntax.
Require Import DeepSpec.core.SynthesisStmt.

Section FUNC_FUNC.
  Open Scope positive_scope.

  Context`{TheLayerSpec : LayerSpecClass}.  (* TODO: this line will go away when we fix the type of HyperMem. *)
  Context`{HM : HyperMem}.

  Let genv_t := genv.

  Definition func_spec_record returns :=
    (GetHighData * (* trace * *) tp_ft returns)%type.
  Definition mk_func_spec_record returns m (*t*) f : func_spec_record returns :=
    ((m (*, t *)), f).
  Definition sf_mem {returns}(r : func_spec_record returns) := (*fst*) (fst r).
  (*Definition sf_trace {returns}(r : func_spec_record returns) := snd (fst r).*)
  Definition sf_return {returns}(r : func_spec_record returns) := snd r.

  (* Statement desugaring returns a record, here we translate that into a func_spec_record (which is actually a pair, because the mcertikos framework didn't use a record here).*)
  Definition stmt_to_func_spec_record {tmp returns}
      (s : spec_env_t tmp -> GetHighData -> stmt_spec_record returns)
      se m
  := let s' := s se m
     in mk_func_spec_record returns s'.(ss_mem) (*s'.(ss_trace)*) s'.(ss_return).

  Record func_output_equivalent {returns} (j:meminj) (ml:mem) v
      (r : func_spec_record returns) : Prop := mk_func_output_equivalent{
(*    fo_mem : mem_match j (sf_mem r) ml; *)
    fo_return : function_return_dec returns = true -> cval_match v (ht_cval (sf_return r));
    fo_ft_cond : ht_ft_cond (sf_return r)
  }.
(*  Global Arguments fo_mem     {_ _ _ _ _} _. *)
  Global Arguments fo_return  {_ _ _ _ _} _ _.
  Global Arguments fo_ft_cond {_ _ _ _ _} _.

  Definition synth_func_pure f := synth_stmt_pure f.(FC_body).

  (* We use fancy dependent types for hetrogeneously typed lists of arguments, and variable-arity functions. *)
  Definition param_folder p (htp : hyper_type_pair)
    := (Pos.succ (fst p), AList.set (fst p) htp (snd p)).
  Definition param_env params ident_start
    := snd (fold_left param_folder params (ident_start, AList.empty)).
  Fixpoint param_func_type (params : list hyper_type_pair) T : Type
    := match params with
       | nil => T
       | htp :: l => tp_ft htp -> param_func_type l T
       end.

  Fixpoint abs_param_func' params T :
    forall ident_start tmp, SpecTree.t tmp ->
      (SpecTree.t (snd (fold_left param_folder params (ident_start, tmp)))
        -> T) ->
      param_func_type params T :=
    match params as p
    return forall ident_start tmp, SpecTree.t tmp ->
      (SpecTree.t (snd (fold_left param_folder p (ident_start, tmp))) -> T) ->
      param_func_type p T with
    | nil => fun _ _ se t => t se
    | htp :: l => fun ident_start tmp se t =>
      fun f => abs_param_func' l T
                 (Pos.succ ident_start) (AList.set ident_start htp tmp)
                 (SpecTree.set ident_start htp f se) t
    end.
  Definition abs_param_func {params T} ident_start :
      (SpecTree.t (param_env params ident_start) -> T) ->
      param_func_type params T :=
    abs_param_func' params T ident_start (AList.empty) SpecTree.empty.

  Fixpoint hlist_param_func' params T (hlist : HList tp_ft params) :
    forall ident_start tmp, SpecTree.t tmp ->
      (SpecTree.t (snd (fold_left param_folder params (ident_start, tmp)))
        -> T) -> T :=
    match hlist in (HList _ p)
    return forall ident_start tmp, SpecTree.t tmp ->
      (SpecTree.t (snd (fold_left param_folder p (ident_start, tmp)))
        -> T) -> T with
    | HNil => fun _ _ se t => t se
    | HCons htp l f rest => fun ident_start tmp se t =>
      hlist_param_func' l T rest
        (Pos.succ ident_start) (AList.set ident_start htp tmp)
        (SpecTree.set ident_start htp f se) t
    end.
  Definition hlist_param_func {params T} ident_start
      (t : SpecTree.t (param_env params ident_start) -> T)
      (hlist : HList tp_ft params) : T
    := hlist_param_func' params T hlist ident_start _ SpecTree.empty t.

  Definition fn_params_folder p (htp : hyper_type_pair)
    := (Pos.succ (fst p), (fst p, tp_ty htp) :: (snd p)).
  Definition fn_params_fold params ident_start
    := rev' (snd (fold_left fn_params_folder params (ident_start, nil))).

  Definition synth_func_wellformed (f : function_constr) : Type :=
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    synth_stmt_wellformed f.(FC_body) f.(FC_ident_start) param_tmp.

  (* Naming convention, synth_func_ are the synthesis functions returning various things,
     in this case returing a Clight function. *)
  Definition synth_func_func (f : function_constr) : function :=
    let dest := f.(FC_ident_start) in
    let ret_ty := tp_ty f.(FC_returns) in
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    let body := synth_stmt_stmt f.(FC_body) dest param_tmp in {|
      fn_return := ret_ty;
      (*fn_callconv := cc_default;*)
      fn_params := fn_params_fold f.(FC_params) f.(FC_param_ident_start);
      fn_temps := (dest, ret_ty) :: synth_stmt_locals f.(FC_body) dest param_tmp;
      fn_locals := nil;
      fn_body := if function_return_dec f.(FC_returns)
                    then Ssequence body (Sreturn (Some xH))
                   else Ssequence body (Sreturn None)
    |}.

  (* return just the return value *)
  Definition synth_func_spec_ret (f : function_constr)
    (wf : synth_func_wellformed f)
    : param_func_type f.(FC_params) (machine_env GetHighData -> GetHighData -> tp_ft f.(FC_returns)) :=
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    abs_param_func f.(FC_param_ident_start)
      (fun se me m => synth_stmt_spec_ret me f.(FC_body) f.(FC_ident_start)
                            param_tmp wf se m).
(*
  (* return both return value and new memory. *)
  Definition synth_func_spec (f : function_constr)
    (wf : synth_func_wellformed f)
    : param_func_type f.(FC_params) (GetHighData -> GetHighData * tp_ft f.(FC_returns)) :=
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    abs_param_func f.(FC_param_ident_start)
      (fun se m => synth_stmt_spec_cont f.(FC_body) f.(FC_ident_start)
                                        param_tmp wf se m
                     _ (fun r => (r.(ss_mem), r.(ss_return)))).
*)
  (* return both return value and new memory, monadic version. *)
  Definition synth_func_spec_opt (f : function_constr)
    (wf : synth_func_wellformed f)
    (* r (k : func_spec_record f.(FC_returns) -> option r) *)
    : param_func_type f.(FC_params)
        (machine_env GetHighData -> DS (tp_ft (FC_returns f))) :=
        (* (GetHighData -> option r) := *)
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    abs_param_func f.(FC_param_ident_start)
      (fun se me => synth_stmt_spec_opt me f.(FC_body) f.(FC_ident_start)
                                       param_tmp wf se).

  (* TODO: May need [ht_ft_cond] of the arguments for VC dischanging to go through.
     In the part of mcertikos that we have reimplemented so far this didn't happen because all ints
     were loop variables (so they automatically have bounded ranges) but that will not hold in general. *)
  Definition synth_func_cond (f : function_constr)(wf : synth_func_wellformed f) :=
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    abs_param_func f.(FC_param_ident_start) (fun se me m =>
      (*fix: *) (* senv_cond se -> *)
      if function_return_dec f.(FC_returns) then
        synth_stmt_veri_cond me f.(FC_body) f.(FC_ident_start) param_tmp wf se m /\
        ht_valid_ft_cond
          (synth_stmt_spec_ret me f.(FC_body) f.(FC_ident_start)
            param_tmp wf se m)
      else
        synth_stmt_veri_cond me f.(FC_body) f.(FC_ident_start) param_tmp wf se m
      ).

  (* TODO: this might also need a senv_cond condition, don't know. (There is a curried se somewhere in here.) *)
  Definition synth_func_obligation (f : function_constr)(wf : synth_func_wellformed f) :=
    let param_tmp := param_env f.(FC_params) f.(FC_param_ident_start) in
    abs_param_func f.(FC_param_ident_start)
      (fun se me d => synth_stmt_obligation me f.(FC_body) f.(FC_ident_start) param_tmp wf se d).

  Fixpoint spec_environment  (htps : list hyper_type_pair)
         : forall (args : HList tp_ft htps) i (rest_typ : AList.t hyper_type_pair) (rest : SpecTree.t rest_typ), SpecTree.t (snd (fold_left param_folder htps (i, rest_typ))) :=
    match  htps
    return forall (args : HList tp_ft htps) i (rest_typ : AList.t hyper_type_pair) (rest : SpecTree.t rest_typ), SpecTree.t (snd (fold_left param_folder htps (i, rest_typ)))
    with
      | nil => fun args ident rest_typ rest => rest
      | htp::htps => fun args i rest_typ rest =>
         spec_environment htps (hlist_tl args) (Pos.succ i) (AList.set i htp rest_typ) (SpecTree.set i _ (hlist_hd args) rest)
    end.

  Fixpoint apply_param_func {params T} :
      param_func_type params T -> HList tp_ft params -> T :=
    match params with
    | nil => fun t _ => t
    | htp :: l => fun f hlist => @apply_param_func l T
                                   (f (hlist_hd hlist)) (hlist_tl hlist)
    end.

  Lemma apply_abs' 
  : forall T htps i tmp (se : SpecTree.t tmp)  
           (f : SpecTree.t (snd (fold_left param_folder htps (i, tmp))) -> T) 
           args, 
     apply_param_func (abs_param_func' htps T i tmp se f) args
   = f (spec_environment htps args i _ se).
  Proof.
  induction htps; intros.
  + reflexivity.
  + simpl.
    rewrite IHhtps.
    reflexivity.
  Qed.

  Lemma apply_abs 
  : forall htps T i 
           (f : SpecTree.t (param_env htps i) -> T) 
           args, 
     apply_param_func (abs_param_func i f) args
   = f (spec_environment htps args i _ (SpecTree.empty)).
  Proof.
    intros.
    apply apply_abs'.
  Qed.  

End FUNC_FUNC.
