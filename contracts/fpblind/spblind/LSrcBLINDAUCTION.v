(* WARNING: This file is generated by Edsger, the DeepSEA compiler.
            All modification will be lost when regenerating. *)
(* Module spblind.LSrcBLINDAUCTION for spblind.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import spblind.EdsgerIdents.
Require Import spblind.DataTypes.
Require Import spblind.DataTypeOps.
Require Import spblind.DataTypeProofs.


Require Import spblind.LayerBLINDAUCTION.

Require Import cclib.Integers.
Require Import cclib.Coqlib.
Require Import cclib.Maps.

Require Import backend.Options.
Require Import backend.AST.
Require Import backend.phase.Clike.Language.
Require Import backend.phase.MiniC.Language.
Require Import backend.Cop.
Require Import backend.Ctypes.
Require Import backend.Compiled.
Require Import backend.Compiled.
Require Import backend.Globalenvs.
Require Import backend.Glue.


Section EdsgerGen.

Definition BlindAuction_initialize_done : function := 
	mkfunction
	BlindAuction_initialize_cfun.(fn_return)
	BlindAuction_initialize_cfun.(fn_params)
	BlindAuction_initialize_cfun.(fn_temps)
	BlindAuction_initialize_cfun.(fn_body).

Definition BlindAuction_transferb_done : function := 
	mkfunction
	BlindAuction_transferb_cfun.(fn_return)
	BlindAuction_transferb_cfun.(fn_params)
	BlindAuction_transferb_cfun.(fn_temps)
	BlindAuction_transferb_cfun.(fn_body).

Definition BlindAuction_reveal_done : function := 
	mkfunction
	BlindAuction_reveal_cfun.(fn_return)
	BlindAuction_reveal_cfun.(fn_params)
	BlindAuction_reveal_cfun.(fn_temps)
	BlindAuction_reveal_cfun.(fn_body).

Definition BlindAuction_bid_done : function := 
	mkfunction
	BlindAuction_bid_cfun.(fn_return)
	BlindAuction_bid_cfun.(fn_params)
	BlindAuction_bid_cfun.(fn_temps)
	BlindAuction_bid_cfun.(fn_body).

Definition BlindAuction_withdraw_done : function := 
	mkfunction
	BlindAuction_withdraw_cfun.(fn_return)
	BlindAuction_withdraw_cfun.(fn_params)
	BlindAuction_withdraw_cfun.(fn_temps)
	BlindAuction_withdraw_cfun.(fn_body).

Definition BlindAuction_auctionEnd_done : function := 
	mkfunction
	BlindAuction_auctionEnd_cfun.(fn_return)
	BlindAuction_auctionEnd_cfun.(fn_params)
	BlindAuction_auctionEnd_cfun.(fn_temps)
	BlindAuction_auctionEnd_cfun.(fn_body).

Definition ge : genv := new_genv (
	(var_BlindAuction__beneficiary_ident, unpair_ty tint_U)::
	(var_BlindAuction__biddingEnd_ident, unpair_ty tint_U)::
	(var_BlindAuction__revealEnd_ident, unpair_ty tint_U)::
	(var_BlindAuction__ended_ident, unpair_ty tint_bool)::
	(var_BlindAuction__bids_ident, unpair_ty thash_Bid_HASH_Z_Bid)::
	(var_BlindAuction__highestBidder_ident, unpair_ty tint_U)::
	(var_BlindAuction__highestBid_ident, unpair_ty tint_U)::
	(var_BlindAuction__secondBid_ident, unpair_ty tint_U)::
	(var_BlindAuction__revealed_ident, unpair_ty thash_int_HASH_Z_bool)::
	(var_BlindAuction__amountOf_ident, unpair_ty thash_int_HASH_Z_Z)::nil)
	nil
	(
	(Int.repr 3835887894, BlindAuction_initialize_done):: (* 0xe4a30116 = "initialize(uint256,uint256)" *)

	(Int.repr 1808565993, BlindAuction_transferb_done):: (* 0x6bcc86e9 = "transferb(uint256,uint256,uint256)" *)

	(Int.repr 2471022327, BlindAuction_reveal_done):: (* 0x9348cef7 = "reveal(uint256,uint256)" *)

	(Int.repr 1162488499, BlindAuction_bid_done):: (* 0x454a2ab3 = "bid(uint256)" *)

	(Int.repr 1020253707, BlindAuction_withdraw_done):: (* 0x3ccfd60b = "withdraw()" *)

	(Int.repr 707064940, BlindAuction_auctionEnd_done):: (* 0x2a24f46c = "auctionEnd()" *)
nil)
	None.


End EdsgerGen.

(*change into extract directory*)
Cd "/Users/sxysun/Desktop/CertiK/DeepSEA/contracts/fpblind/spblind/extraction".

(* Avoid name clashes *)
Extraction Blacklist List String Int.

Separate Extraction
    positive global_abstract_data_type  (* This line forces Coq to generate some files even if they are not used, to make the makefile work correctly. *)
    Glue.full_compile_genv
    ge.
