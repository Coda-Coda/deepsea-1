Require Import BinPosDef.  
Require Import List.
Require Import Bool.
Require Import BinInt.
Require Import backend.AST.
Require Import backend.MemoryModel.
Require Import backend.Values.HighValues.
Require Import cclib.Integers.
Require Import cclib.Maps.
Require Import backend.Statements.StmtMiniC.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.MachineModel.
Require Import DeepSpec.core.Cval.
Require Import DeepSpec.core.SEnv.
Require Import DeepSpec.core.HyperType.
Require Import DeepSpec.core.HyperTypeInst.
Require Import DeepSpec.core.MemoryModel.
Require Import DeepSpec.lib.OProp.
Require Import DeepSpec.lib.Monad.ContOptionMonad.
Generalizable Variables tp tpl tpr tpo.  
Section VARIABLE.
  
  Context `{HM : HyperMem}.
  Definition variable tp := ltype_pair tp.
  
  Definition offset_zero := Int256.zero.
  
  Record variable_prf {tp}`{HyperTypeImpl tp}(v : variable tp) : Prop
      := mk_var_prf {
    
    VARoffset_zero : v.(ltype_ident) = Field Global (ident_ext_base (v.(ltype_ident)));
    VARltype : HyperLType v
  }.
  Global Arguments VARoffset_zero {_ _ _} _.
  Global Arguments VARltype       {_ _ _} _.
  
  
  Class variable_passthrough_prf {tp}`{HyperTypeImpl tp}(v : variable tp) : Prop
    := mk_var_pt_prf {
    
    VAR_relate_impl_eq j d1 d2 :
      relate_AbData j d1 d2 ->
      (v.(ltype_get) d1) = (v.(ltype_get) d2);
    
    VAR_relate_impl_update j d1 d2 :
      relate_AbData j d1 d2 ->
      forall f,
        relate_AbData j (v.(ltype_set) f d1) (v.(ltype_set) f d2);
    
    VAR_match_impl_update j d m :
      match_AbData d m j ->
      forall f,
        match_AbData (v.(ltype_set) f d) m j;
    
  }.
End VARIABLE.
Coercion create_type_pair_marker := @create_type_marker type_pair.
Section EXPR_CONSTR.
  Context `{HM : HyperMem}.
    
  
  Inductive expr_constr : forall tp {hti : HyperTypeImpl tp}, Type :=
  | ECconst_int : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int.int -> expr_constr tp          
  | ECconst_int256 : forall tp `{HyperTypeImpl tp},
    unpair_ft tp -> Int256.int -> expr_constr tp          
  | ECtempvar : forall tp `{HyperTypeImpl tp},
      ident -> expr_constr tp                         
  | ECbuiltin0 : forall tp `{HyperTypeImpl tp, !HyperBuiltin0Impl tp},
      expr_constr tp  
  | ECbuiltin1 : forall atp tp `{HyperTypeImpl atp, HyperTypeImpl tp, !HyperBuiltin1Impl atp tp},
      expr_constr atp ->  expr_constr tp  
  | ECunop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpo, HyperUnaryImpl op tpo tp},
    expr_constr tpo -> expr_constr tp
                                                       
  | ECbinop : forall tp op `{HyperTypeImpl tp, HyperTypeImpl tpl, HyperTypeImpl tpr,
                             HyperBinaryImpl op tpl tpr tp},
    expr_constr tpl -> expr_constr tpr -> expr_constr tp
  
  
  with lexpr_constr : forall tp `{hti : HyperTypeImpl tp}, Type :=
  | LCvar : forall`{HyperTypeImpl tp}(v : variable tp), lexpr_constr tp
                                                              
  | LCfield : forall tp id `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperFieldImpl tpl tp id},
    lexpr_constr tpl -> lexpr_constr tp
                               
  | LCindex : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIndexImpl tpl tp},
      lexpr_constr tpl -> expr_constr tint_Z32 -> lexpr_constr tp
                               
  | LChash : forall tp `{HyperTypeImpl tp, HyperTypeImpl tpl, !HyperIntHashImpl tpl tp},
      lexpr_constr tpl -> expr_constr tint_U -> lexpr_constr tp
                               
  .
  Fixpoint lexpr_is_ghost `{HyperTypeImpl}(e : lexpr_constr tp) :=
    match e with
    | LCvar _ _ v => v.(ltype_ghost)
    | LCfield _ tid _ _ _ pfieldi e' => lexpr_is_ghost e'
    | LCindex _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    | LChash _ _ _ _ _ e_arr e_idx => lexpr_is_ghost e_arr
    end.
  
  
  Inductive expr_constr_prf : forall `{HyperType tp}, expr_constr tp -> Prop :=
  
  | ECPconst_int256 : forall`{HyperType tp} f i (fc : ht_ft_cond f)
    (rel : ht_cval f = CVval (Vint i)), expr_constr_prf (ECconst_int256 _ f i)
  | ECPtempvar : forall`{ht : HyperType tp} t, expr_constr_prf (ECtempvar tp t)
  | ECPbuiltin0 : forall `{HyperBuiltin0 (LayerSpec := LayerSpec) tp, !HyperType tp}, expr_constr_prf (ECbuiltin0 tp)
  | ECPbuiltin1 : forall atp `{HyperBuiltin1 (LayerSpec := LayerSpec) atp tp, !HyperType atp, !HyperType tp} e,
    expr_constr_prf e ->
    expr_constr_prf (ECbuiltin1 atp tp e)
  | ECPunop : forall op `{HyperUnaryOp op tpo tp, ht : !HyperType tp,
                          hto : !HyperType tpo} e,
    expr_constr_prf e -> expr_constr_prf (ECunop _ op e)
  | ECPbinop : forall op `{HyperBinaryOp op tpl tpr tp, ht : !HyperType tp,
                           htl : !HyperType tpl, htr : !HyperType tpr} el er,
    expr_constr_prf el -> expr_constr_prf er ->
    expr_constr_prf (ECbinop _ op el er)
  with lexpr_constr_prf : forall`{HyperType tp}, lexpr_constr tp -> Prop :=
  | LCPvar : forall`{HyperType tp} var (v_prf : variable_prf var),
    lexpr_constr_prf (LCvar var)
  | LCPfield : forall id `{HyperField tpl tp id, !HyperType tpl, !HyperType tp}
                      e (ec : lexpr_constr_prf e),
    lexpr_constr_prf (LCfield tp id e)
  | LCPindex : forall`{HyperIndex tpl tp, !HyperType tp, !HyperType tpl} e idx,
    lexpr_constr_prf e -> expr_constr_prf idx ->
    lexpr_constr_prf (LCindex tp e idx)
  | LCPhash : forall`{HyperIntHash tpl tp, !HyperType tp, !HyperType tpl} e idx,
    lexpr_constr_prf e -> expr_constr_prf idx ->
    lexpr_constr_prf (LChash tp e idx)
  .
  
  Inductive expr_constr_passthrough_prf :
      forall`{HyperTypeImpl tp}, expr_constr tp -> Prop :=
  | ECPPconst_int : forall`{hti : HyperTypeImpl tp} f i,
    expr_constr_passthrough_prf (ECconst_int tp f i)
  | ECPPtempvar : forall`{hti : HyperTypeImpl tp} t,
    expr_constr_passthrough_prf (ECtempvar tp t)
  | ECPPunop : forall tp op `{htu : HyperUnaryPassthrough op tpo tp} e,
    expr_constr_passthrough_prf e -> expr_constr_passthrough_prf (ECunop tp op e)
  | ECPPbinop : forall tp op `{htb : HyperBinaryPassthrough op tpl tpr tp} el er,
    expr_constr_passthrough_prf el -> expr_constr_passthrough_prf er ->
    expr_constr_passthrough_prf (ECbinop tp op el er)
  with lexpr_constr_passthrough_prf :
      forall`{HyperTypeImpl tp}, lexpr_constr tp -> Prop :=
  | LCPPvar : forall`{hti : HyperTypeImpl tp} var (v_prf : variable_passthrough_prf var),
    lexpr_constr_passthrough_prf (LCvar var)
  | LCPPfield : forall id `{htf : HyperFieldPassthrough tpl tp id} e,
    lexpr_constr_passthrough_prf e ->
    lexpr_constr_passthrough_prf (LCfield tp id e)
  | LCPPindex : forall `{htd : HyperIndexPassthrough tpl tp} e idx,
    lexpr_constr_passthrough_prf e -> expr_constr_passthrough_prf idx ->
    lexpr_constr_passthrough_prf (LCindex tp e idx)
  .
End EXPR_CONSTR.
Section STMT_CONSTR.
  
  
  
  Context `{HM : HyperMem}.
  Definition function_return_dec returns
    := match tp_ty returns with
       | Tvoid => false
       | _ => true
       end.
  Require Import DeepSpec.lib.Monad.StateMonadOption.
  Import MonadNotation.
  Open Scope monad_scope.
  Definition DS := osT GetHighData.
  Instance Monad_DS : Monad DS := MosT GetHighData.
  Instance MonadLaws_DS : MonadLaws (Monad_stateT GetHighData Monad_option).
  Proof. Admitted.
  Instance MonadState_DS : MonadState GetHighData DS.
  apply MonadState_stateT.
  apply Monad_option.
  Defined.
  Instance MonadZero_DS : MonadZero DS.
  apply MonadZero_stateT.
  apply Monad_option.
  apply Zero_option.
  Defined.


Inductive CEIP_color :=
  | CEIP_green
  | CEIP_orange.

  (*  The underlay interface for a primitive is a Coq Prop, a relation from memory etc to the final value.
      But we want a Coq function. So the primitive record contains the function.

      Then the class primitive_exec_prf says that the primitive is equivalent to the specification in the
      underlay interface.
   *)
  Record primitive (argt : list hyper_type_pair)(ret : hyper_type_pair)
      : Type := mk_prim {
    PRIMident : ident;
                    
    
    PRIMghost : bool;                 
    PRIMpure  : bool;                 
    PRIMargt_marker : type_marker argt;
    PRIMret_marker : type_marker ret;
    (* PRIMceip_color_before/after/B relate to reentrancy tracking using the Checks Effects 
    Interactions pattern. They represent the safe state(s) the prim can be executed
    in and the resulting CEIP_color afterwards.
    In particular they should be one of the following:
      
      When a prim has only safe commands:
      CEIP_green, CEIP_green
      CEIP_orange, CEIP_orange
      
      When a prim has commands only safe before reentrancy:
      CEIP_green, CEIP_green
      CEIP_green, CEIP_green (repeated)
      

      When a prim has a command that introduces reentrancy:
      CEIP_green, CEIP_orange
      CEIP_green, CEIP_orange (repeated)

    The relevant values are filled in automatically.
      *)
      PRIMceip_color_before : CEIP_color;
      PRIMceip_color_after : CEIP_color;

    (* PRIMsem_opt, the "monadic" version, combines PRIMcond and PRIMsem into one thing.
       There is a verification condition below saying that they must be equivalent;
       the synthesis use them interchangably for convenience. *)
    PRIMcond : HList tp_ft argt -> machine_env GetHighData -> GetHighData -> Prop;
    PRIMsem_opt : HList tp_ft argt -> machine_env GetHighData -> DS (tp_ft ret)
  }.
  
  Global Arguments PRIMident {_ _} _.
  
  Global Arguments PRIMghost {_ _} _.
  Global Arguments PRIMpure  {_ _} _.
  Global Arguments PRIMcond {_ _} _ _ _.
  
  
  Global Arguments PRIMsem_opt {_ _} _ _.
  
  Fixpoint to_typelist argt :=
    match argt with
    | nil => Ctypes.Tnil
    | cons x xs => Ctypes.Tcons (tp_ty x) (to_typelist xs)
    end.
  
  Class primitive_prf {argt returns}(prim : primitive argt returns) := mk_prim_prf {
    
  
    PRIMret_cond :
      forall args me,
        ht_list_ft_cond args ->
        ht_list_valid_ft_cond args ->
      forall (s : GetLowData) v,
        evalStateT (prim.(PRIMsem_opt) args me) s = ret v ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s ->
        
        
        ht_ft_cond v /\
        (function_return_dec returns = true -> ht_valid_ft_cond v);
      
  
  
    PRIMis_pure :
      prim.(PRIMpure) = true ->
      forall args me d,
      forall d', execStateT (prim.(PRIMsem_opt) args me) d = ret d' ->
                 d = d';
    PRIMinv :
      prim.(PRIMpure) = false ->
      forall args me,
        ht_list_ft_cond args ->
        ht_list_valid_ft_cond args ->
      forall (s s' : GetLowData),
        execStateT (prim.(PRIMsem_opt) args me) s = ret s' ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s ->
        high_level_invariant (CompatDataOps := cdataOpsLow) s'
   }.
  
  Class primitive_exec_prf {argt ret}(prim : primitive argt ret) := mk_prim_exec_prf {
    
    
  }.
  Class primitive_passthrough_prf {argt ret}(prim : primitive argt ret)
    := mk_prim_pt_prf {
    
    
  }.
  Definition expr_constr_list :=
    HList (fun htp => expr_constr (tp_type_pair htp)).
  Definition expr_constr_prf_conj {ls}(es : expr_constr_list ls) :=
    HList_fold_right_nodep (fun htp e p =>
        p /\ { ht : HyperType (tp_type_pair htp) | expr_constr_prf (H := ht) e })
      True es.
  Definition expr_constr_passthrough_prf_conj {ls}(es : expr_constr_list ls) :=
    HList_fold_right_nodep (fun htp e p => p /\ expr_constr_passthrough_prf e)
      True es.
  Inductive cmd_constr : forall (returns : hyper_type_pair), Type :=
  | CCskip : cmd_constr void_unit_pair                      
  | CClet : forall {r}`{HyperTypeImpl tp}(id : ident),
    cmd_constr (mk_hyper_type_pair tp) -> cmd_constr r -> cmd_constr r
                                            
  | CCload : forall`{HyperTypeImpl tp},
    lexpr_constr tp -> cmd_constr (mk_hyper_type_pair tp)
                                                           
  | CCstore : forall`{HyperTypeImpl tp},
    lexpr_constr tp -> expr_constr tp -> cmd_constr void_unit_pair
                                        
  | CCsequence : forall {r},
    cmd_constr void_unit_pair -> cmd_constr r -> cmd_constr r 
  | CCifthenelse : forall {r},
    expr_constr tint_bool -> cmd_constr r -> cmd_constr r -> cmd_constr r
                                                           
  | CCfor : forall id_it id_end : ident,
    expr_constr tint_Z32 -> expr_constr tint_Z32 -> cmd_constr void_unit_pair ->
    cmd_constr void_unit_pair                             
  | CCfirst : forall {r}(id_it id_end id_dest : ident),
    expr_constr tint_Z32 -> expr_constr tint_Z32 ->
    cmd_constr int_bool_pair ->
    cmd_constr r -> cmd_constr r -> cmd_constr r         
  | CCfold : forall `{HyperTypeImpl tp}(id_it id_end id_recur id_dest : ident),
    expr_constr tint_Z32 -> expr_constr tint_Z32 -> expr_constr tp ->
    cmd_constr (mk_hyper_type_pair tp) ->
    cmd_constr (mk_hyper_type_pair tp)                     
  | CCcall : forall {argt ret},
    primitive argt ret -> expr_constr_list argt -> cmd_constr ret
  | CCcallmethod : forall {argt ret},
    int256 -> (* the address of the external object/contract to call *)
    primitive argt ret -> expr_constr_list argt -> cmd_constr ret                                              
  | CCtransfer : expr_constr tint_U -> expr_constr tint_U -> cmd_constr void_unit_pair
                                                              
  | CCyield : forall`{HyperTypeImpl tp},
    expr_constr tp -> cmd_constr (mk_hyper_type_pair tp)
                            
  
  | CCconstr : forall`{HyperTypeImpl tp}(fld_ids : list ident) fld_tps,
    lexpr_constr tp -> expr_constr_list fld_tps ->
    list_curried_type fld_tps (unpair_ft tp) ->
    cmd_constr void_unit_pair
  | CCassert : cmd_constr int_bool_pair -> cmd_constr void_unit_pair
                                          
  | CCdeny : cmd_constr int_bool_pair -> cmd_constr void_unit_pair
                                          
  
                                            
  | CCpanic : forall tp`{HyperTypeImpl tp},
    cmd_constr (mk_hyper_type_pair tp)
                         
  
  | CCrespec : forall {r} tmp', cmd_constr r ->
    forall spec : spec_env_t tmp' -> DS (tp_ft r),
    cmd_constr r                         
  | CCrespec_opt : forall {r} tmp', cmd_constr r ->
    forall spec :  machine_env GetHighData -> spec_env_t tmp' -> DS (tp_ft r), 
      
    cmd_constr r                         
  .
  Inductive cmd_constr_passthrough_prf : forall ret, cmd_constr ret -> Prop :=
  | CCPPskip : cmd_constr_passthrough_prf _ CCskip
  | CCPPlet : forall {r}`{hti : HyperTypeImpl tp}(id : ident) c1 c2,
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) c1 ->
    cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CClet id c1 c2)
  | CCPPload : forall`{hti : HyperTypeImpl tp} e,
    lexpr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCload e)
  | CCPPstore : forall`{hti : HyperTypeImpl tp} el er,
    lexpr_constr_passthrough_prf el -> expr_constr_passthrough_prf er ->
    cmd_constr_passthrough_prf _ (CCstore el er)
  | CCPPsequence : forall {r} c1 c2,
    cmd_constr_passthrough_prf _ c1 -> cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CCsequence c1 c2)
  | CCPPifthenelse : forall {r} e c1 c2,
    expr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf r c1 -> cmd_constr_passthrough_prf r c2 ->
    cmd_constr_passthrough_prf r (CCifthenelse e c1 c2)
  | CCPPfor : forall id_it id_end e1 e2 c,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    cmd_constr_passthrough_prf _ c ->
    cmd_constr_passthrough_prf _ (CCfor id_it id_end e1 e2 c)
  | CCPPfirst : forall {r} id_it id_end id_dest e1 e2 c3 c4 c5,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    cmd_constr_passthrough_prf _ c3 ->
    cmd_constr_passthrough_prf r c4 -> cmd_constr_passthrough_prf r c5 ->
    cmd_constr_passthrough_prf r (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
  | CCPPfold : forall `{hti : HyperTypeImpl tp} id_it id_end id_recur id_dest e1 e2 e3 c,
    expr_constr_passthrough_prf e1 -> expr_constr_passthrough_prf e2 ->
    expr_constr_passthrough_prf e3 ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) c ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp)
                               (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
  | CCPPcall : forall {argt ret} prim arg,
    expr_constr_passthrough_prf_conj arg ->
    primitive_passthrough_prf prim ->
    cmd_constr_passthrough_prf _ (@CCcall argt ret prim arg)
  
                         
  | CCPPtransfer : forall e1 e2,
    expr_constr_passthrough_prf e1 ->
    expr_constr_passthrough_prf e2 ->
    cmd_constr_passthrough_prf void_unit_pair (CCtransfer e1 e2)
                               
  | CCPPyield : forall`{hti : HyperTypeImpl tp} e,
    expr_constr_passthrough_prf e ->
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCyield e)
  | CCPPconstr : forall`{HyperTypeImpl tp} fld_ids fld_tps el flds constr
      `{hcp : !HyperConstructorPassthrough tp fld_tps constr},
    lexpr_constr_passthrough_prf el -> expr_constr_passthrough_prf_conj flds ->
    cmd_constr_passthrough_prf _ (CCconstr fld_ids fld_tps el flds constr)
  | CCPPassert : forall c,
    cmd_constr_passthrough_prf _ c -> cmd_constr_passthrough_prf _ (CCassert c)
  | CCPPdeny : forall c,
    cmd_constr_passthrough_prf _ c -> cmd_constr_passthrough_prf _ (CCdeny c)
  | CCPPpanic : forall tp`{HyperTypeImpl tp},
    cmd_constr_passthrough_prf (mk_hyper_type_pair tp) (CCpanic tp)
  | CCPPrespec : forall {r} tmp' c spec,
    cmd_constr_passthrough_prf r (CCrespec tmp' c spec)
  | CCPPrespec_opt: forall {r} tmp' c spec,
    cmd_constr_passthrough_prf r (CCrespec_opt tmp' c spec)
  .
  
  Record function_constr : Type := mk_function_constr {
    FC_ident_start : ident;
    FC_param_ident_start := Pos.succ FC_ident_start;
    FC_params : list hyper_type_pair;
    FC_returns : hyper_type_pair;
    FC_body : cmd_constr FC_returns
  }.
  Definition function_constr_passthrough_prf f : Prop :=
    cmd_constr_passthrough_prf f.(FC_returns) f.(FC_body).
End STMT_CONSTR.
Coercion create_list_marker := fun A => @create_type_marker (list A).
Coercion create_hyper_type_pair_marker := @create_type_marker hyper_type_pair.
Section CONSTR_PRF.
  Context `{HM : HyperMem}.
  
  Inductive cmd_constr_prf (is_realizing : bool) :
      forall ret (htr : HyperType (tp_type_pair ret)), cmd_constr ret -> Prop :=
  | CCPskip : cmd_constr_prf is_realizing _ _ CCskip
  | CCPlet : forall r `{ht : HyperType tp} htr id c1 c2,
    function_return_dec (mk_hyper_type_pair tp) = true ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht c1 ->
    cmd_constr_prf is_realizing r htr c2 ->
    cmd_constr_prf is_realizing r htr (CClet id c1 c2)
  | CCPload : forall `{ht : HyperType tp, hbv : !HyperByValueType tp} e
                
                     (ec : lexpr_constr_prf e),
    
    (is_realizing && lexpr_is_ghost e = false) ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht (CCload e)
  | CCPstore : forall `{ht : HyperType tp, hbv : !HyperByValueType tp} el er
                      (ecl : lexpr_constr_prf el)(ecr : expr_constr_prf er),
    
    (is_realizing || lexpr_is_ghost el = true) ->
    cmd_constr_prf is_realizing _ _ (CCstore el er)
  | CCPsequence : forall r htr c1 c2,
    cmd_constr_prf is_realizing _ _ c1 ->
    cmd_constr_prf is_realizing r htr c2 ->
    cmd_constr_prf is_realizing r htr (CCsequence c1 c2)
  | CCPifthenelse : forall r htr e c_true c_false
                           (ec : expr_constr_prf e),
    cmd_constr_prf is_realizing r htr c_true ->
    cmd_constr_prf is_realizing r htr c_false ->
    cmd_constr_prf is_realizing r htr (CCifthenelse e c_true c_false)
  | CCPfor : forall id_it id_end e1 e2 c
                   (ec1 : expr_constr_prf e1)(ec2 : expr_constr_prf e2),
    cmd_constr_prf is_realizing _ _ c ->
    cmd_constr_prf is_realizing _ _ (CCfor id_it id_end e1 e2 c)
  | CCPfirst : forall r htr id_it id_end id_dest e1 e2 c3 c4 c5,
    expr_constr_prf e1 -> expr_constr_prf e2 ->
    cmd_constr_prf is_realizing int_bool_pair int_bool c3 ->
    cmd_constr_prf is_realizing r htr c4 ->
    cmd_constr_prf is_realizing r htr c5 ->
    cmd_constr_prf is_realizing r htr
                   (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
  | CCPfold : forall `{ht : HyperType tp} id_it id_end id_recur id_dest
      e1 e2 e3 c (ec1 : expr_constr_prf e1)(ec2 : expr_constr_prf e2)
      (ec3 : expr_constr_prf e3),
    function_return_dec (mk_hyper_type_pair tp) = true ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht c ->
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht
                   (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
  | CCPcall : forall argt ret htr prim arg
      (IHprim : @primitive_prf _ _ argt ret prim)
      (IHprim_exec : primitive_exec_prf prim)
      (IHprim_b : primitive_passthrough_prf prim)
      (ecs : expr_constr_prf_conj arg),
    
    
      
      is_realizing || prim.(PRIMpure) = true ->
    cmd_constr_prf is_realizing ret htr (CCcall prim arg)
  
  | CCPyield : forall `{ht : HyperType tp} e (ec : expr_constr_prf e),
    cmd_constr_prf is_realizing (mk_hyper_type_pair tp) ht (CCyield e)
  | CCPconstr : forall `{ht : HyperType tp} fld_ids fld_tps el flds constr
      `{hc : !HyperConstructor tp fld_ids fld_tps constr}
      (ecl : lexpr_constr_prf el)(fldc : expr_constr_prf_conj flds)
      (flds_byvalue : HList
        (fun htp : hyper_type_pair =>
           HyperByValueType (tp_type_pair htp)) fld_tps),
    
    (is_realizing || lexpr_is_ghost el = true) ->
    (length fld_ids <= Z.to_nat Int256.modulus)%nat ->
    cmd_constr_prf is_realizing _ _ (CCconstr fld_ids fld_tps el flds constr)
  | CCPtransfer : forall e1 e2,
    expr_constr_prf e1 ->
    expr_constr_prf e2 ->
    cmd_constr_prf is_realizing _ _ (CCtransfer e1 e2)
                   
  
  | CCPassert : forall c, cmd_constr_prf is_realizing _ _ c -> cmd_constr_prf is_realizing _ _ (CCassert c)
  | CCPdeny : forall c, cmd_constr_prf is_realizing _ _ c -> cmd_constr_prf is_realizing _ _ (CCdeny c)
  | CCPpanic : forall `{ht : HyperType} , cmd_constr_prf is_realizing _ _ (CCpanic tp )
  | CCPrespec : forall r htr tmp' c spec,
    
    cmd_constr_prf is_realizing r htr c ->
    cmd_constr_prf is_realizing r htr (CCrespec tmp' c spec)
  | CCPrespec_opt : forall r htr tmp' c spec,
    
    is_realizing = true ->
    cmd_constr_prf is_realizing r htr c ->
    cmd_constr_prf is_realizing r htr (CCrespec_opt tmp' c spec)
  .
  Record function_constr_prf (is_realizing : bool) (f : function_constr) : Prop := mk_function_constr_prf {
    
    FC_returns_ht : HyperType (tp_type_pair f.(FC_returns));
    FC_body_prf : cmd_constr_prf is_realizing f.(FC_returns) FC_returns_ht f.(FC_body)
  }.
  
  
  
End CONSTR_PRF.

Section CEIP.
  Context `{HM : HyperMem}.

Fixpoint contains_balance_read
    `{HyperTypeImpl}
    (e : expr_constr tp) {struct e} : bool :=
    match e with
  | ECconst_int _ _ _ _ => false
  | ECconst_int256 _ _ _ _ => false
  | ECtempvar _ _ _ => false
  | ECbuiltin0 _ _ possibly_balance_read =>
    match (@Hbuiltin0 LayerSpec _ possibly_balance_read) with
      | Bselfbalance => true
      | _ => false
    end
  | ECbuiltin1 _ _ _ _ possibly_balance_read e' => 
    match (@Hbuiltin1 LayerSpec _ _ possibly_balance_read) with
      | Bbalance => true
      | _ => false
    end
    || contains_balance_read e'
  | ECunop _ _ _ _ _ _ e' => contains_balance_read e'
  | ECbinop _ _ _ _ _ _ _ _ el er =>
    (contains_balance_read el) || (contains_balance_read er)
  end%type.

Inductive CEIP_prf :
(**  Abbreviations:
      CEIP = Checks-Effects-Interactions Pattern
      prf = proof
      r = return type (of the command)
      r1 = return type 1
      c1 = command 1
      color = reentrancy safety state
      e = expression
*)
  forall ret,
    CEIP_color -> cmd_constr ret -> CEIP_color -> Prop :=
    (* In the type of CEIP_prf, the first occurance of CEIP_color corresponds to the CEIP_color before executing the command, and the last CEIP_color corresponds to the CEIP_color after executing the command. *)
| CEIP_skip :
    forall {color},
      CEIP_prf _ color CCskip color
      (* "skip" leaves color unchaged *)
| CEIP_let :
    forall {color1} {color2} {color3} r `{ht : HyperType tp} id c1 c2,
      CEIP_prf (mk_hyper_type_pair tp) color1 c1 color2 ->
      CEIP_prf r color2 c2 color3 ->
      CEIP_prf r color1 (CClet id c1 c2) color3
      (* "let" leaves color as the color from running c1 then c2 *)
| CEIP_load : 
    forall `{ht : HyperType tp} e,
      CEIP_prf _ CEIP_green (CCload e) CEIP_green
      (* "load" can only be run safely from CEIP_green, and leaves it color as CEIP_green *)
      (* Note that e might contain a balance read, but there is no need to check in addition because load is already only safe in the same situation as a balance read is. *)
| CEIP_store :
    forall `{ht : HyperType tp} el er,
      CEIP_prf _ CEIP_green (CCstore el er) CEIP_green
      (* "store" can only be run safely from CEIP_green, and leaves it color as CEIP_green *)
      (* Note that e might contain a balance read, but there is no need to check in addition because store is already only safe in the same situation as a balance read is. *)
| CEIP_sequence :
    forall {color1} {color2} {color3} r (c1 : cmd_constr void_unit_pair) (c2 : cmd_constr r),
      CEIP_prf void_unit_pair color1 c1 color2 ->
      CEIP_prf r color2 c2 color3 ->
      CEIP_prf r color1 (CCsequence c1 c2) color3
      (* "sequence" leaves color as the color from running c1 then c2 *)      

(* "ifthenelse" is split into the five allowable cases (rather than using a match statement) to simplify the type-checking. *)
(* Here we assume the worst case scenario, I guess it would be nice to be able to analyse the truth value of e here. *)
(* "ifthenelse" leaves color as the color from assuming the worst case scenario out of the commands in the branches *)
| CEIP_ifthenelse1 : (* CEIP_green CEIP_green CEIP_green *)
  forall r e c_true c_false,
    CEIP_prf r CEIP_green c_true CEIP_green ->
    CEIP_prf r CEIP_green c_false CEIP_green ->
    CEIP_prf r CEIP_green (CCifthenelse e c_true c_false) CEIP_green
| CEIP_ifthenelse2 : (* CEIP_green CEIP_orange CEIP_green *)
  forall r e c_true c_false,
    CEIP_prf r CEIP_green c_true CEIP_orange ->
    CEIP_prf r CEIP_green c_false CEIP_green ->
    CEIP_prf r CEIP_green (CCifthenelse e c_true c_false) CEIP_orange      
| CEIP_ifthenelse3 : (* CEIP_green CEIP_green CEIP_orange *)
  forall r e c_true c_false,
    CEIP_prf r CEIP_green c_true CEIP_green ->
    CEIP_prf r CEIP_green c_false CEIP_orange ->
    CEIP_prf r CEIP_green (CCifthenelse e c_true c_false) CEIP_orange      
| CEIP_ifthenelse4 : (* CEIP_green CEIP_orange CEIP_orange *)
  forall r e c_true c_false,
    CEIP_prf r CEIP_green c_true CEIP_orange ->
    CEIP_prf r CEIP_green c_false CEIP_orange ->
    CEIP_prf r CEIP_green (CCifthenelse e c_true c_false) CEIP_orange      
| CEIP_ifthenelse5 : (* CEIP_orange CEIP_orange CEIP_orange *)
  forall r e c_true c_false,
    contains_balance_read e = false ->
    CEIP_prf r CEIP_orange c_true CEIP_orange ->
    CEIP_prf r CEIP_orange c_false CEIP_orange ->
    CEIP_prf r CEIP_orange (CCifthenelse e c_true c_false) CEIP_orange      
| CEIP_for1 :
forall id_it id_end e1 e2 c,
  contains_balance_read e1 || contains_balance_read e2 = true ->
  CEIP_prf _ CEIP_green c CEIP_green ->
  CEIP_prf _ CEIP_green (CCfor id_it id_end e1 e2 c) CEIP_green
  (* This is slightly overly restrictive, for example a for loop that actually only loops once and contains a transferEth call will not be allowed. *)
(* "for" probably loops through c many times, so to be safe we assume it can only be run safely if c is a command which results in color (and starts from color). Then the loop as a whole is only safe if started from color and will result in a color state. *)
| CEIP_for2 :
forall {color} id_it id_end e1 e2 c,
  contains_balance_read e1 = false ->
  contains_balance_read e2 = false ->
  CEIP_prf _ color c color ->
  CEIP_prf _ color (CCfor id_it id_end e1 e2 c) color
  (* This is slightly overly restrictive, for example a for loop that actually only loops once and contains a transferEth call will not be allowed. *)
(* "for" probably loops through c many times, so to be safe we assume it can only be run safely if c is a command which results in color (and starts from color). Then the loop as a whole is only safe if started from color and will result in a color state. *)
| CEIP_first1 :
    forall r id_it id_end id_dest e1 e2 c3 c4 c5,
      contains_balance_read e1 || contains_balance_read e2 = true ->
      CEIP_prf int_bool_pair CEIP_green c3 CEIP_green ->
      CEIP_prf r CEIP_green c4 CEIP_green ->
      CEIP_prf r CEIP_green c5 CEIP_green ->
      CEIP_prf r CEIP_green
                    (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
                    CEIP_green
            (* Slightly over-restrictive, like for. Also the command that executes when the loop exits could move color from CEIP_green to CEIP_orange, but this, being overly restrictive would not allow that (fairly unlikely) situation. *)
      (* With "first", similarly to "for" we assume all the commands (which may be looped) result in color (and start from color). So the overall first-loop is only safe if started from color and will result in a color state. *)
| CEIP_first2 :
forall {color} r id_it id_end id_dest e1 e2 c3 c4 c5,
  contains_balance_read e1 = false ->
  contains_balance_read e2 = false ->
  CEIP_prf int_bool_pair color c3 color ->
  CEIP_prf r color c4 color ->
  CEIP_prf r color c5 color ->
  CEIP_prf r color
                (CCfirst id_it id_end id_dest e1 e2 c3 c4 c5)
                color
        (* Slightly over-restrictive, like for. Also the command that executes when the loop exits could move color from CEIP_green to CEIP_orange, but this, being overly restrictive would not allow that (fairly unlikely) situation. *)
  (* With "first", similarly to "for" we assume all the commands (which may be looped) result in color (and start from color). So the overall first-loop is only safe if started from color and will result in a color state. *)
| CEIP_fold1 :
  forall `{ht : HyperType tp} id_it id_end id_recur id_dest e1 e2 e3 c,
    contains_balance_read e1 || contains_balance_read e2 || contains_balance_read e3 = true ->
    CEIP_prf _ CEIP_green c CEIP_green ->
    CEIP_prf _
      CEIP_green
      (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
      CEIP_green
      (* Similarly to CEIP_for, this is overly restrictive. *)
  (* With "fold", similarly to "for" and "first" we assume all the commands (which may be looped) result in color (and start from color). So the overall fold is only safe if started from color and will result in a color state. *)
| CEIP_fold2 :
  forall `{ht : HyperType tp} {color} id_it id_end id_recur id_dest e1 e2 e3 c,
    contains_balance_read e1 = false ->
    contains_balance_read e2 = false ->
    contains_balance_read e3 = false ->
    CEIP_prf _ color c color ->
    CEIP_prf _
      color
      (CCfold id_it id_end id_recur id_dest e1 e2 e3 c)
      color
      (* Similarly to CEIP_for, this is overly restrictive. *)
(* With "fold", similarly to "for" and "first" we assume all the commands (which may be looped) result in color (and start from color). So the overall fold is only safe if started from color and will result in a color state. *)
| CEIP_call1 :
    forall {color1} {color2} r argt prim arg,
      color1 = prim.(PRIMceip_color_before argt r) -> color2 = prim.(PRIMceip_color_after argt r) -> CEIP_prf r color1 (CCcall prim arg) color2
| CEIP_call2 :
    forall r argt prim arg,
      CEIP_prf r CEIP_orange (CCcall prim arg) CEIP_orange
      -> CEIP_prf r CEIP_green (@CCcall LayerSpec argt r prim arg) CEIP_green
    (* "call" should result in the same color as the primitive being called, and be safe to call in the same circumstances as the primitive is called.
        The primitives will have automatically generated fields color_before/after to describe the safe ways in which prim can be called.
        Since having before and after of CEIP_orange implies having a before and after of CEIP_green this is 
        captured here in CEIP_call2 so that it is not necessary to capture both options within the prim color_before/after fields, rather just CEIP_orange (before and after)
        in the primitive color_before/after fields and this rule will capture the possibility that the prim is safe for CEIP_green
        (before and after) as well. This simplifies the situation somewhat. *)
| CEIP_yield1 :
  forall `{ht : HyperType tp} e,
    contains_balance_read e = true ->
      CEIP_prf _ CEIP_green (CCyield e) CEIP_green
    (* If a balance is read in the expression "yield" is only safe before potential reentrancy. *)
| CEIP_yield2 :
  forall {color} `{ht : HyperType tp} e,
    contains_balance_read e = false ->
    CEIP_prf _ color (CCyield e) color
    (* "yield" retains the current color as long as the expression does not read a balance. *)
| CEIP_constr :
    forall `{ht : HyperType tp} fld_ids fld_tps el flds constr,
      CEIP_prf _ CEIP_green (CCconstr fld_ids fld_tps el flds constr) CEIP_green
      (* "constr" assigns to storage so can only be called if CEIP_green, and does not introduce the possiblity of reentrancy itself. *)
| CEIP_transfer :
    forall e1 e2,
      CEIP_prf _ CEIP_green (CCtransfer e1 e2) CEIP_orange
      (* What this is all about, "transfer" can only be called in a CEIP_green state and DOES introduce potential reentrancy. *)
| CEIP_assert :
    forall {color1} {color2} c,
      CEIP_prf _ color1 c color2 -> CEIP_prf _ color1 (CCassert c) color2
    (* "assert" results in the same color (and can be called safely in the same situations as) the command c *)
| CEIP_deny :
    forall {color1} {color2} c,
      CEIP_prf _ color1 c color2 -> CEIP_prf _ color1 (CCdeny c) color2
  (* "deny" results in the same color (and can be called safely in the same situations as) the command c *)
(* | CEIP_panic : *) (* Unclear how best to handle this case. A safe way is to have no rule for CEIP_panic, this does cause issues when using the "address(0x10032) <:" syntax in layer definitions, related to respec_opt (at least)  *)
| CEIP_respec : (* Similarly to respec_opt, below,  *)
    forall {color1} {color2} r tmp' c spec,
      CEIP_prf r color1 c color2 -> 
      CEIP_prf r color1 (CCrespec tmp' c spec) color2
  (* "respec" results in the same color (and can be called safely in the same situations as) the command c *)
| CEIP_respec_opt : (* This rule likely needs revision. Currently `c` would be (CCpanic _returnType_) which does not give sufficient information to determine if the CEIP is being followed. *)
    forall {color1} {color2} r tmp' c spec,
      CEIP_prf r color1 c color2 -> 
      CEIP_prf r color1 (CCrespec_opt tmp' c spec) color2
  (* "respec_opt" results in the same color (and can be called safely in the same situations as) the command c *)
.

Definition CEIP_function_prf 
    (color_before : CEIP_color)
    (f : function_constr)
    (color_after : CEIP_color) :=
       CEIP_prf f.(FC_returns) color_before f.(FC_body) color_after.

End CEIP.