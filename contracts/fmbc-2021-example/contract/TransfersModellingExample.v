(* Please note that this file serves as an example only of the idea for simplifying the modelling of Ether transfer. It is not a complete nor entirely correct definition of reachable states of the contract. *)

(* The most relevant code starts from around line 60. *)

Require Import contract.DataTypes.
Require Import contract.DataTypeOps.
Require Import lib.Monad.StateMonadOption.
Require Import cclib.Maps.
Require Import cclib.Integers.
Require Import ZArith.
Require Import core.HyperTypeInst.
Require Import backend.MachineModel.
Require Import core.MemoryModel.
Require Import DeepSpec.lib.Monad.RunStateTInv.
Require Import Lia.
Require Import contract.GenericMachineEnv.
Require Import contract.LayerETH_layer.
Require Import contract.LayerCONTRACT.
Require Import contract.SingleTransferCheck.
Require Import contract.GenericMachineEnv.

Require Import List.
Import ListNotations.

Section Example.
Definition addr := int256.
Context 
(update_balances_with_single_transfer : addr -> addr -> int256 -> (int256 -> int256) -> addr -> int256)
(contract_address : int256)
(initial_balances : addr -> int256)
(origin: addr)
(caller: addr)
(callvalue : int256)
(coinbase : int256)
(timestamp : int256)
(number : int256)
(balance : int256 -> int256)
(blockhash : int256 -> int256)
(chainid : int256)
(address_always_accepts_funds : addr -> bool)
(ValidInitialMachineEnv : machine_env (global_abstract_data_type) -> Prop)
(ValidNextMachineEnvBeforeContractExecution : machine_env (global_abstract_data_type) -> machine_env (global_abstract_data_type) -> Prop )
{HmemOps: MemoryModelOps mem}
{memModelOps : MemoryModelOps mem}.
Instance GlobalLayerSpec : LayerSpecClass := {
  memModelOps := memModelOps;
  GetHighData := global_abstract_data_type 
}.

Definition me_example := GenericMachineEnv.generic_machine_env coinbase timestamp number blockhash chainid
origin contract_address caller callvalue initial_balances
address_always_accepts_funds.

Definition update_me_balance me_before current_balances : machine_env global_abstract_data_type :=
  GenericMachineEnv.generic_machine_env (me_coinbase me_before) (me_timestamp me_before) (me_number me_before) (me_blockhash me_before) (me_chainid me_before) (me_origin me_before) (me_address me_before) (me_caller me_before) (@me_callvalue global_abstract_data_type me_before) current_balances address_always_accepts_funds.

(* ------------------------- *)
(* Most relevant code below here. *)



(* The following updates balances from a list of transfers, it does not need to consider the case where the list is longer than one because of the proof `length_evidence`. *)
Obligation Tactic := idtac.
Program Definition  update_balances_from_transfer_list (transfers: list Transfer) (length_evidence : (length transfers <= 1)%nat) (previous_balances : int256 -> int256) (a : addr) := match transfers with
| [] => previous_balances a
| [t] => update_balances_with_single_transfer contract_address (recipient t) (amount t) previous_balances a
| (h :: i :: t) as l => _ (* Coq allows us to discharge this case. *)
end.
Next Obligation. (* This is the case where transfers = h :: i :: t *)
intros.
exfalso. (* This should be impossible, so we try prove False. *)
rewrite <- Heq_transfers in length_evidence. simpl in length_evidence. lia. (* These lines make use of the proof that the list is of length at most one and yet also of length two to derive a contradiction. *)
Defined.


(* This Lemma converts information that the safeExample command was run into a proof that the resulting transfers list is of length at most one. *)
Lemma process_proof {d d' r} :
ETH_successful_transfers d = [] ->  
(match runStateT (Contract_safeExample_opt me_example) d with
| Some (r'out, d'out) => d'out = d' /\ r'out = r
| None => False
end) -> (length (ETH_successful_transfers d') <= 1)%nat.
Proof.
intros.
destruct (runStateT (Contract_safeExample_opt me_example) d) eqn:Case; [|contradiction].
 destruct p.
 apply SingleTransferCheck.Contract_safeExample_opt_single_transfer (* Here is where the ;lemma proved automatically showing that safeExample only results in a list of transfers of length at most one is used.*)
 with (coinbase:=coinbase) (timestamp:=timestamp) (number:=number) (blockhash:=blockhash) (chainid:=chainid) (origin:=origin) (contract_address:=contract_address) (caller:=caller) (callvalue:=callvalue) (initial_balances:=initial_balances) (address_always_accepts_funds:=address_always_accepts_funds) (d:=d) (result:=r).
 assumption.
 unfold me_example in Case.
 unfold SingleTransferCheck.me.
 destruct H0.
 subst.
 assumption. 
Qed.

(*

The following is a illustration of how this all would tie together when defining a ReachableState predicate.

Please note that this is not an actual definition of ReachableState, it merely illustrates the idea of using `length_evidence` so that the case for multiple transfers can be safely ignored.

It is directly using the automatically proved lemma via `process_proof`. 

*)

Inductive ReachableState : global_abstract_data_type -> machine_env (global_abstract_data_type) -> Prop :=
  | contractStep_safeExample : forall d d' r
    (proof1 : ReachableState d me_example)
    (proof2 : ETH_successful_transfers d = [])
    (proof3 : match runStateT (Contract_safeExample_opt me_example) d with
       | Some (r'out, d'out) => d'out = d' /\ r'out = r
       | None => False
       end),

    ReachableState d'    
    
    (update_me_balance me_example( update_balances_from_transfer_list (ETH_successful_transfers d') (process_proof proof2 proof3) (me_balance me_example d)))
.

End Example.