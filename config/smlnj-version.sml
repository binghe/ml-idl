(* smlnj-version.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * Helper program to dump out SML/NJ version numbers.
 *)

val _ = let
      val i2a = Int.toString
      val s = TextIO.openOut "smlnj-versions"
      fun pr l = TextIO.output(s, concat l)
      in
        case #version_id Compiler.version
	 of [major] => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=0\n"];
	      pr["SMLNJ_PATCH_VERSION=0\n"])
	  | [major, minor] => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=", i2a minor, "\n"];
	      pr["SMLNJ_PATCH_VERSION=0\n"])
	  | (major::minor::patch::_) => (
	      pr["SMLNJ_MAJOR_VERSION=", i2a major, "\n"];
	      pr["SMLNJ_MINOR_VERSION=", i2a minor, "\n"];
	      pr["SMLNJ_PATCH_VERSION=", i2a patch, "\n"])
	(* end case *);
	TextIO.closeOut s
      end;
