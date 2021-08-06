Definition reentrancy_tracking {returns} (c :cmd_constr returns) :=
    let log_safe_even_after_reentrancy :=
    d_for_reentrancy_safety_check <- get ;;
    put (me_reentrancy_safety_tracker me SafeEvenAfterReentrancy 
        d_for_reentrancy_safety_check) in
        
    let log_only_safe_before_reentrancy :=
    d_for_reentrancy_safety_check <- get ;;
    put (me_reentrancy_safety_tracker me OnlySafeBeforeReentrancy
        d_for_reentrancy_safety_check) in
    
    let log_allows_reentrancy :=
    d_for_reentrancy_safety_check <- get ;;
    put (me_reentrancy_safety_tracker me AllowsReentrancy
        d_for_reentrancy_safety_check)
    in

  match c with

    (* SafeEvenAfterReentrancy *)
    | CCskip 
    | CClet _ _ _ _ _ _ 
    | CCsequence _ _ _ 
    | CCifthenelse _ _ _ _ 
    | CCfor _ _ _ _ _ 
    | CCfold _ _ _ _ _ _ _ _ _ _ 
    | CCcall _ _ _ _ 
    | CCyield _ _ _ 
    | CCassert _ 
    | CCdeny _ 
    | CCpanic _ _ 
    | CCrespec_opt _ _ _ _
      => log_safe_even_after_reentrancy

    (* OnlySafeBeforeReentrancy *)
    | CCload _ _ _ 
    | CCstore _ _ _ _ 
    | CCconstr _ _ _ _ _ _ _ 
    | CCrespec _ _ _ _ 
    | CCfirst _ _ _ _ _ _ _ _ _ 
      => log_only_safe_before_reentrancy
    
    (* AllowsReentrancy *)
    | CCtransfer _ _ 
 (* | CCcall_ext *)
      => log_allows_reentrancy
    end.

Fixpoint synth_stmt_spec_opt_main (enable_reentrancy_tracking_flag : bool) {returns}(c : cmd_constr returns) dest tmp :
  synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> DS (tp_ft returns) :=
    
  match c with
  | CCskip => let without_reentrancy_tracking := (fun wf se => ret tt) in
              if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CClet _r tp _ id c1 c2 =>
    let without_reentrancy_tracking :=(
    let htp := mk_hyper_type_pair tp in
    (fun (wf : let tmp' := AList.set id (mk_hyper_type_pair tp) tmp in
            ((tmp ! id ~~~ None) * dest_not_exist dest tmp') *
            (synth_stmt_wellformed c1 id tmp * synth_stmt_wellformed c2 dest tmp'))
       se =>
       spec1 <- (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c1 id tmp (cadr wf) se);;
       let se' := SpecTree.set id htp spec1 se in
       (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c2 dest _ (cddr wf) se'))
    ) in
    if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCload tp _ e => 
  let without_reentrancy_tracking :=(
  fun wf se =>
    gets (synth_lexpr_spec me tmp e wf se).(ltype_get)
    ) in
    if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCstore _ _ el er => let without_reentrancy_tracking := (
    fun wf se =>
    let f := synth_expr_spec me tmp er (cdr wf) se in
    modify ((synth_lexpr_spec me tmp el (car wf) se).(ltype_set) f)
    ) in
    if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCsequence _ c1 c2 =>
  let without_reentrancy_tracking :=(
    fun (wf : synth_stmt_wellformed c1 dest tmp * synth_stmt_wellformed c2 dest tmp)
      se =>
      (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c1 dest tmp (car wf) se) ;;
      (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c2 dest tmp (cdr wf) se)
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCifthenelse _ e c_true c_false =>
  let without_reentrancy_tracking :=(
    fun (wf : synth_expr_wellformed tmp e *
             (synth_stmt_wellformed c_true dest tmp * synth_stmt_wellformed c_false dest tmp))
        se =>
      if synth_expr_spec me tmp e (car wf) se
      then synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c_true  dest tmp (cadr wf) se
      else synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c_false dest tmp (cddr wf) se
    ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCfor id_it id_end e1 e2 c3 =>
  let without_reentrancy_tracking :=(
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
            let tmp'' := AList.set id_it int_Z32_pair tmp' in
            ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
             dest_not_exist dest tmp'')) *
            (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
             synth_stmt_wellformed c3 dest tmp'')))
      se =>
      let start := synth_expr_spec me tmp e1 (cadr wf) se in
      let bound := synth_expr_spec me tmp e2 (caddr wf) se in
      let initial_se := SpecTree.set id_end int_Z32_pair bound se in
      Ziteri (for_step_opt initial_se start
                         (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c3 dest _ (cdddr wf)))
           (bound - start)
           (ret tt)
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCfirst _ id_it id_end id_dest e1 e2 c3 c4 c5 =>
  let without_reentrancy_tracking :=(
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
              let tmp'' := AList.set id_it int_Z32_pair tmp' in
              ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
               ((tmp'' ! id_dest ~~~ None) * (dest_not_exist dest tmp'' *
                ((synth_stmt_pure c3 ~~~ true)))))) *
              (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
               (synth_stmt_wellformed c3 id_dest tmp'' *
               (synth_stmt_wellformed c4 dest tmp'' *
                synth_stmt_wellformed c5 dest tmp'')))))
      se =>
      let start := (synth_expr_spec me tmp e1 (cadr wf) se) in
      let bound := (synth_expr_spec me tmp e2 (caddr wf) se) in
      m <- get;;
      first_map
      (fun n =>
         synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      start bound;;
      first_spec
      (fun n =>
         synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c3 id_dest _ (cadddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      (fun n =>
         synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c4 dest _ (caddddr wf)
           (SpecTree.set id_it int_Z32_pair n
           (SpecTree.set id_end int_Z32_pair bound se)))
      (fun n =>
         synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c5 dest _ (cdddddr wf)
           (SpecTree.set id_it int_Z32_pair bound 
           (SpecTree.set id_end int_Z32_pair n se)))
      start
      bound
      m
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCfold tp _ id_it id_end id_recur id_dest e1 e2 e3 c4 =>
  let without_reentrancy_tracking :=(
    let htp := mk_hyper_type_pair tp in
    fun (wf : let tmp' := AList.set id_end int_Z32_pair tmp in
              let tmp'' := AList.set id_it int_Z32_pair tmp' in
              let tmp''' := AList.set id_recur (mk_hyper_type_pair tp) tmp'' in
              ((tmp ! id_end ~~~ None) * ((tmp' ! id_it ~~~ None) *
              ((tmp'' ! id_recur ~~~ None) * ((tmp''' ! id_dest ~~~ None) *
              (dest_not_exist dest tmp''' * (synth_stmt_pure c4 ~~~ true)))))) *
              (synth_expr_wellformed tmp e1 * (synth_expr_wellformed tmp e2 *
              (synth_expr_wellformed tmp e3 * synth_stmt_wellformed c4 id_dest tmp'''))))
        se =>
    let start := synth_expr_spec me tmp e1 (cadr wf) se in
    let bound := synth_expr_spec me tmp e2 (caddr wf) se in
    let init := synth_expr_spec me tmp e3 (cadddr wf) se in
    let initial_se := SpecTree.set id_end int_Z32_pair bound se in
    oZiteri (fold_step_opt initial_se start
                          (synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c4 id_dest _ (cddddr wf)))
          (bound - start)
          init
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCcall argt ret prim args =>
  let without_reentrancy_tracking :=(
    fun wf se =>
      let argv := map2_synth_expr_spec args se wf in
      prim.(PRIMsem_opt) argv me
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCtransfer e1 e2 =>
  let without_reentrancy_tracking :=(
    fun (wf : synth_expr_wellformed tmp e1 *
              synth_expr_wellformed tmp e2)
        se =>
      d <- get ;;
      let (success , d') :=
          me_transfer me
                      (synth_expr_spec me tmp e1 (car wf) se)
                      (synth_expr_spec me tmp e2 (cdr wf) se)
                      d in
      if (Int256.eq success Int256.one)
      then put d'
      else mzero
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCyield tp _ e =>
  let without_reentrancy_tracking :=(
    fun wf se =>
      ret (synth_expr_spec me tmp e wf se)
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCconstr _ _ _ _ el flds constr =>
  let without_reentrancy_tracking :=(
    fun wf se =>
      let l := synth_lexpr_spec me tmp el (car wf) se in
      modify (l.(ltype_set)
              (apply_curried_func
                  constr
                  (map2_synth_expr_spec flds se (cdr wf))))
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCassert c =>
  let without_reentrancy_tracking :=(
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c dest tmp (cdr wf) se;;
      guard v
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCdeny c =>
  let without_reentrancy_tracking :=(
    fun (wf: (synth_stmt_pure c ~~~ true) * synth_stmt_wellformed c dest tmp) se =>
      v <- synth_stmt_spec_opt_main enable_reentrancy_tracking_flag c dest tmp (cdr wf) se;;
      guard (negb v)
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCpanic _ _ => fun _ _ =>
  let without_reentrancy_tracking :=( mzero
  ) in if enable_reentrancy_tracking_flag then ( reentrancy_tracking c ;; without_reentrancy_tracking) else without_reentrancy_tracking
  | CCrespec _ tmp' c spec =>
  let without_reentrancy_tracking :=(
      fun (wf : ((tmp ~~~ tmp') * (synth_stmt_pure c ~~~ true)) *
              synth_stmt_wellformed c dest tmp) se =>
        m <- get;;
        v <- spec (iso_f (caar wf) se);;
        put m;;
        ret v
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking
  | CCrespec_opt _ tmp' c spec =>
  let without_reentrancy_tracking :=(
    fun (wf : (tmp ~~~ tmp') * synth_stmt_wellformed c dest tmp) se =>
      spec me (iso_f (car wf) se)
  ) in if enable_reentrancy_tracking_flag then (fun wf se => reentrancy_tracking c ;; without_reentrancy_tracking wf se) else without_reentrancy_tracking

end %alist.

Definition synth_stmt_spec_opt {returns}(c : cmd_constr returns) dest tmp :
synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> DS (tp_ft returns) := 
synth_stmt_spec_opt_main false c dest tmp.

Definition synth_stmt_spec_opt_with_reentrancy_tracking {returns}(c : cmd_constr returns) dest tmp :
synth_stmt_wellformed c dest tmp -> spec_env_t tmp -> DS (tp_ft returns) := 
synth_stmt_spec_opt_main true c dest tmp.



(* Also, the following three definitions (from different files) are relevant. 
   standard_me_reentrancy_tracker below is used as the definition of me_reentrancy_safety_tracker. *)

Inductive reentrancy_safety_state :=
  | Safe_no_reentrancy
  | Safe_with_potential_reentrancy
  | Unsafe
.

Definition standard_me_reentrancy_tracker := (fun tag d =>
let safety_state_d := FixedSupplyToken__reentrancy_safety_state d in    
match safety_state_d with
| Transfers.Safe_no_reentrancy => 
  match tag with
  | SafeEvenAfterReentrancy => d (* Still Safe_no_reentrancy *)
  | OnlySafeBeforeReentrancy => d (* Still Safe_no_reentrancy *)
  | AllowsReentrancy => d_with_new_safety_tag Transfers.Safe_with_potential_reentrancy d
  end
| Transfers.Safe_with_potential_reentrancy =>
  match tag with
  | SafeEvenAfterReentrancy => d (* Still Safe_with_potential_reentrancy *)
  | OnlySafeBeforeReentrancy => d_with_new_safety_tag Transfers.Unsafe d
  | AllowsReentrancy => d_with_new_safety_tag Transfers.Unsafe d
  end
| Transfers.Unsafe => d (* Still Unsafe *)
end
).