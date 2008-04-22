(* generate-runtime-glue.sml
 *
 *)


structure GenerateRuntimeGlue : sig

    val generate : (string * IIL.iil) -> unit

  end = struct

    structure I = IIL
    structure A = Atom
    structure S = AtomMap
    structure F = Format

    open Util

    fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())


    fun generate (file, iil) = let
	  val I.IIL{decls, clib_name=SOME lib, clib_version, clib_date, ...} = iil
	  val proto_file = replace_extension (file, "-lib", "hxx")
	  val clib_file = replace_extension (file, "-lib", "cxx")
	  fun dump_lib_hxx os = let
		val symb = toCPPSymbol proto_file
		fun outputOper (I.Operation (name,oper)) = out os [
			"ML_Value ml_stub_", Atom.toString name, " (ML_Context *, ML_Value);"
		      ]
		  | outputOper _ = ()
		in
		  out os ["#ifndef ", symb];
		  out os ["#define ", symb];
		  out os [];
		  out os ["#include \"SMLNJ/base.h\""];
		  out os [];
		  app outputOper decls;
		  out os [];
		  out os ["#endif /* !", symb, " */"]
		end      
	  fun dump_lib_cxx os = let
		fun outputOper (I.Operation(name, oper)) = 
		      out os [F.format "\tML_CFunction(ml_stub_%s, \"%s\")," [F.ATOM name, F.ATOM name]]
		  | outputOper _ = ()
		in
		  out os ["#include \"SMLNJ/c-library.hxx\""];
		  out os ["#include \"", proto_file, "\""];
		  out os [];
		  out os ["static ML_CFunction fnTbl[] = {"];
		  app outputOper decls;
		  out os ["    };"];
		  out os [];
		  out os ["ML_CLibraryDef ", lib, "Lib("];
		  out os ["\t(CLibInfo_t *)0,"];
		  out os ["\t(ML_CLibInit_t)0,"];
		  out os ["\tfnTbl);"]
		end
	  in
	    Verbose.message1 ["Generating glue files"];
	    withOutputFile (FILE_CXX, proto_file, dump_lib_hxx);
	    withOutputFile (FILE_CXX, clib_file, dump_lib_cxx)
	  end

  end
