(* WARNING: This file is generated by Edsger, the DeepSEA compiler.
            All modification will be lost when regenerating. *)
(* Module swaps_general.LSrcEVMOPCODE for swaps_general.ds *)
Require Import BinPos.
Require Import DeepSpec.Runtime.
Require Import swaps_general.EdsgerIdents.
Require Import swaps_general.DataTypes.
Require Import swaps_general.DataTypeOps.
Require Import swaps_general.DataTypeProofs.


Require Import swaps_general.LayerEVMOPCODE.

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

Definition ge : genv := new_genv (nil)
	nil
	(nil)
	None.


End EdsgerGen.

(*change into extract directory*)
Cd "/Users/lemontree/Dropbox/Year4-2/CPSC528/DeepSEA/contracts/swaps/swaps_general/extraction".

(* Avoid name clashes *)
Extraction Blacklist List String Int.

Separate Extraction
    positive global_abstract_data_type  (* This line forces Coq to generate some files even if they are not used, to make the makefile work correctly. *)
    Glue.full_compile_genv
    ge.
