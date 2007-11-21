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

  fun dump_lib_hxx (I.IIL {decls,clib_name=SOME(lib),clib_version,clib_date,...}) os = let
          fun outputOper (I.Operation (name,oper)) = 
                out os [F.format "  ML_CFunction(ml_stub_%s, \"%s\")" 
				 [F.ATOM (name),F.ATOM (name)]]
            | outputOper _ = ()
        in
          out os ["ML_CFunction fnTbl[] = {"];
          app outputOper decls;
	  out os ["}"]
        end
    | dump_lib_hxx _ _ = Error.error ["IDL file missing required information (e.g. clib_name)"]

  fun generate (file, iil as (I.IIL {symtable,decls,...})) = let
    val clib_file = replace_extension (file,"-lib","hxx")
  in
    Verbose.message1 ["Generating glue files"];
    withOutputFile (FILE_C,clib_file,dump_lib_hxx iil)
  end

end
