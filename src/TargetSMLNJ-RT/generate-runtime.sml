(* generate-runtime.sml
 *
 * CHANGES:
 * 6/18/01 - moved the typedef and cpp_quote stuff to the header
 *)

(* add enumeration types *)

structure GenerateRuntime : sig end =
  struct

    structure I = IIL
    structure A = Atom
    structure S = AtomMap
    structure F = Format
    
    open Util

    fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

    val mapP = List.mapPartial
    
  (* for customization *)
    val ml_val = "ML_Value"
    val ml_context = "ML_Context"
    val ml_fun_stub = "ml_stub_"
    val ml_arg_stub = "ml_arg_"
    val ml_op_stub = "ml_op_"

    fun ml_tuple n = "ML_Tuple<" ^ (Int.toString n) ^ ">"
    fun downcast (ty, obj) = "(*(reinterpret_cast<" ^ ty ^ " *> (&(" ^ obj ^ "))))"
    fun ml_sub (i,n,v) = downcast(ml_tuple n, v) ^ ".Get(ctx, " ^ (Int.toString (i+1)) ^ ")"

    local 
      val i = ref (0)
    in
    fun gensym () = "ml_tmp_"^(Int.toString (!i)) before i := (!i) + 1
    end
    
    val glob_symtable = ref (NONE) : I.typedef S.map option ref

    fun findType (I.TS_Id (s)) = (case (!glob_symtable)
	   of NONE => fail ["GenerateRuntime.findType uninitialized global symbol table"]
	    | SOME (st) => (case (S.find (st,s))
		 of SOME (I.TypeDef {spec,...}) => findType (spec)
		  | NONE => Error.error ["is not a type"]
		(* end case *))
	  (* end case *))
      | findType (t) = t

  (* 
   * Given a name and a type, create a C declaration for that name (minus the ;)
   *)
    fun mk_declaration (n,I.TS_Id (s)) = concat [A.toString (s)," ",n]
      | mk_declaration (n,I.TS_Real64) = "double " ^ n
      | mk_declaration (n,I.TS_Real32) = "double " ^ n
      | mk_declaration (n,I.TS_Int32) = "int32_t " ^ n
      | mk_declaration (n,I.TS_Int16) = "int32_t " ^ n
      | mk_declaration (n,I.TS_Int8) = "int32_t " ^ n
      | mk_declaration (n,I.TS_Word32) = "uint32_t " ^ n
      | mk_declaration (n,I.TS_Word16) = "uint32_t " ^ n
      | mk_declaration (n,I.TS_Word8) = "uint32_t " ^ n
      | mk_declaration (n,I.TS_Int) = "int " ^ n
      | mk_declaration (n,I.TS_Word) = "unsigned int " ^ n
      | mk_declaration (n,I.TS_Bool) = "bool " ^ n
      | mk_declaration (n,I.TS_Char) = "char " ^ n
      | mk_declaration (n,I.TS_String) = "const char *" ^ n
      | mk_declaration (n,I.TS_Ref spec) = mk_declaration ("*"^n,spec)
      | mk_declaration (n,I.TS_Ptr spec) = mk_declaration ("*"^n,spec)
      | mk_declaration (n,I.TS_Option (I.TS_String)) = "const char *" ^ n
      | mk_declaration (n,I.TS_Option (I.TS_Ref spec)) = mk_declaration ("*"^n,spec)
(*
      | mk_declaration (n,I.TS_Array {spec,size}) = 
	  (case (size) 
	     of I.E_Int (i) => mk_declaration (concat [n,"[",Int32.toString (i),"]"],spec)
	      | _ => mk_declaration ("*"^n,spec))
*)
      | mk_declaration (n,I.TS_StructTag (a)) = concat ["struct ",A.toString (a)," ",n]
      | mk_declaration (n,I.TS_Sml _) = concat [ml_val," ",n]
      | mk_declaration (n,I.TS_App {oper,...}) = mk_declaration (n,oper)
      | mk_declaration (n,I.TS_Dep {spec,...}) = mk_declaration (n,spec)
      | mk_declaration (n,ts) = fail ["GenerateRuntime.mk_declaration",
                                          "unhandled type spec ",IIL.spec_to_string (ts)]

      type rho = (A.atom * I.exp) list
      val p_empty = [] : rho
      fun p_lookup ([],a) = Error.error ["Unbound variable ",A.toString a]
	| p_lookup (((x,y)::xs),a) = if (A.sameAtom (x,a)) then y else p_lookup (xs,a)
      fun p_add (p,(x,y)) = (x,y)::p

      fun str_constant (p,I.E_Int (i)) = Int32.toString (i)
	| str_constant (p,I.E_Id (x)) = str_constant (p,p_lookup (p,x))  (* assume alpha-renaming all the way through *)
	| str_constant (p,I.E_String (x)) = x

      fun makeTuple (results:string list,what:string -> string):string list = (case results
	     of [] => [what ("ML_UNIT")]
	      | [x] => [what (x)]
	      | l => ["  {\n",
		      "    ", ml_val, " vs[] = {", String.concatWith ", " l, "};\n",
		      "    ", ml_tuple (length l), " r = ", ml_tuple (length l), "(ctx, vs);\n",
		      "    ", what ("r"), "\n",
		      "  };"]
	    (* end case *))

  (* 
   * Given two variables and a type, create the code that marshalls 
   * The result is a pair (dl,code) with 'dl' a list of declarations used as temporary variables
   * and code a list of the actual code to perform the marshalling.
   *)
    fun marshallType (fr,to,I.TS_Id (s)) p = marshallType (fr,to,findType (I.TS_Id (s))) p
      | marshallType (fr,to,I.TS_Real64) p = ([],[concat [to," = ",downcast("ML_Real64",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Real32) p = ([],[concat [to," = ",downcast("ML_Real64",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Int32)  p = ([],[concat [to," = ",downcast("ML_Int32",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Int16)  p = ([],[concat [to," = ",downcast("ML_Int32",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Int8)   p = ([],[concat [to," = ",downcast("ML_Int32",fr),".Val();"]])
(*
      | marshallType (fr,to,I.TS_Word32) p = ([],[concat [to," = WORD_MLtoC (",fr,");"]])
      | marshallType (fr,to,I.TS_Word16) p = ([],[concat [to," = WORD_MLtoC (",fr,");"]])
      | marshallType (fr,to,I.TS_Word8)  p = ([],[concat [to," = WORD_MLtoC (",fr,");"]])
*)
      | marshallType (fr,to,I.TS_Int)    p = ([],[concat [to," = ",downcast("ML_Int",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Word)   p = ([],[concat [to," = ",downcast("ML_Int",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Bool)   p = ([],[concat [to," = ",downcast("ML_Int",fr),".Val();"]])
      | marshallType (fr,to,I.TS_Char)   p = ([],[concat [to," = (char) ",downcast("ML_Int",fr),".Val();"]])
      | marshallType (fr,to,I.TS_String) p = ([],[concat [to," = ",downcast("ML_String",fr),".UnsafeCString();"]])
(*
      | marshallType (fr,to,I.TS_Option (I.TS_String)) p = let
	  val n = gensym ()
	  val (d,c) = marshallType (n,to,I.TS_String) p
	in
	  (d,concat ["if (",fr," == OPTION_NONE) { ",to," = NULL; } else { ",ml_val," ",n,"; ",n," = OPTION_get (",fr,"); ", c," };"])
	end
*)
(*
      | marshallType (fr,to,I.TS_Ptr spec) p = (*([],concat [to," = PTR_MLtoC (void,",fr,");"]) *)
	  ([],concat [to," = PTR_MLtoC (",I.spec_to_string (spec),",",fr,");"])
*)
      | marshallType (fr,to,I.TS_Option (I.TS_Ref spec)) p = let
	  val n = gensym ()
	  val n_opt = gensym ()
	  val (d,c) = marshallType (n_opt,n,spec) p
	  val d' = mk_declaration (n,spec)
	in 
	  (d'::d, 
	   List.concat [
	     [concat ["if (",downcast("ML_Option<"^ml_val^">",fr),".isNONE()) {"],
	      concat ["  ", to," = NULL;"],
	      concat ["} else {"],
	      concat ["  ", ml_val," ",n_opt,";"],
	      concat ["  ", n_opt," = ", downcast("ML_Option<"^ml_val^">",fr), ".ValOf(ctx);"]],
	     map (fn x => "  " ^ x) c,
	     [concat ["  ",to," = &",n,";"],
	      concat ["};"]]])
	end
      | marshallType (fr,to,I.TS_Ref spec) p = let
	  val n = gensym () 
	  val (d,c) = marshallType (fr,n,spec) p
	  val d' = mk_declaration (n,spec)
	in
	  (d'::d,c @ [concat [to," = &",n,";"]]) 
	end
(*
      | marshallType (fr,to,I.TS_Array {spec,size}) p = let
	  val (d,c) = marshallType ("<sub i from "^fr^">",to^"[i]",spec) p
	  val size_bnd = str_constant (p,size)
	  val alloc = case (size)
			of I.E_Int _ => ""
			 | _ => concat [to," = (<cast>) malloc (<size> * ",size_bnd,"); "]
	in
	  (d,concat [alloc,"for (int i=0; i < ",size_bnd,"; i++) ",c])
	end
*)
      | marshallType (fr,to,I.TS_Sml _) p = ([],[concat [to," = ",fr, ";"]])
      | marshallType (fr,to,I.TS_App {oper=I.TS_Dep {id,id_spec,spec},app}) p = let
	  val app' = case (app) 
		       of I.E_Id (var) => I.E_String (ml_op_stub^(A.toString (var)))
			| _ => fail ["GenerateRuntime.marshallType","Huh? not a E_Id..."]
	in
	  marshallType (fr,to,spec) (p_add (p,(id,app')))
	end
      | marshallType (fr,to,I.TS_Struct (fields)) p = let
	  val fields = map (fn (I.Fld {spec,name,...}) => (A.toString name,spec)) fields
	  fun mk_marshall ((name,spec),i) = marshallType (ml_sub(i,length fields,fr),"("^to^")."^name,spec) p_empty
	  val marshalled = mapI mk_marshall fields
	  fun proj1 l = map (fn (a,b) => a) l
	  fun proj2 l = map (fn (a,b) => b) l
	in
	  (List.concat (proj1 marshalled),
	   List.concat (proj2 marshalled))
	end
      | marshallType (fr,to,t) p = fail ["GenerateRuntime.marshallType",
					     "unhandled type spec ",IIL.spec_to_string (t)]
      
    fun unmarshallType (fr,to,I.TS_Id (s)) = unmarshallType (fr,to,findType (I.TS_Id (s)))
      | unmarshallType (fr,to,I.TS_Real64) = ([], concat [to, " = ML_Real64(ctx,",fr,");"])
      | unmarshallType (fr,to,I.TS_Real32) = ([], concat [to, " = ML_Real64(ctx,",fr,");"])
      | unmarshallType (fr,to,I.TS_Int32)  = ([], concat [to, " = ML_Int32(ctx,",fr,");"])
      | unmarshallType (fr,to,I.TS_Int16)  = ([], concat [to, " = ML_Int32(ctx,",fr,");"])
      | unmarshallType (fr,to,I.TS_Int8)   = ([], concat [to, " = ML_Int32(ctx,",fr,");"])
(*
      | unmarshallType (fr,to,I.TS_Word32) = ([], concat ["WORD_ALLOC (msp,",to,",",fr,");"])
      | unmarshallType (fr,to,I.TS_Word16) = ([], concat ["WORD_ALLOC (msp,",to,",",fr,");"])
      | unmarshallType (fr,to,I.TS_Word8)  = ([], concat ["WORD_ALLOC (msp,",to,",",fr,");"])
*)
      | unmarshallType (fr,to,I.TS_Int)    = ([], concat [to," = ML_Int(",fr,");"])
      | unmarshallType (fr,to,I.TS_Word)   = ([], concat [to," = ML_Int(",fr,");"])
      | unmarshallType (fr,to,I.TS_Bool)   = ([], concat [to," = ML_Int(",fr,");"])
      | unmarshallType (fr,to,I.TS_Char)   = ([], concat [to," = ML_Int((int) ",fr,");"])
      | unmarshallType (fr,to,I.TS_String) = ([], concat [to," = ML_String(ctx, ",fr,");"])
(*    | unmarshallType (fr,to,I.TS_Option (I.TS_String)) = ([],concat [to," = <to_ml_opt_string> (",fr,");"]) *)
(*    | unmarshallType (fr,to,I.TS_Ptr spec) = ([],concat [to," = PTR_CtoML (",fr,");"]) *)
      | unmarshallType (fr,to,I.TS_Option (I.TS_Ref spec)) = let
	  val (d,c) = unmarshallType ("*("^fr^")",to,spec)
	in 
	  (d,c)
	end
      | unmarshallType (fr,to,I.TS_Ref spec) = let
	  val (d,c) = unmarshallType ("*("^fr^")",to,spec)
	in
	  (d,c)
	end
      | unmarshallType (fr, to, I.TS_Sml _) = ([], concat [to," = ",fr, ";"])
      | unmarshallType (fr ,to, I.TS_Struct fields) = let
	  val n = gensym ()
	  val fields = map (fn (I.Fld {spec,name,...}) => (A.toString name,spec)) fields
	  fun mk_unmarshall ((name, spec), i) = let
		val (l, ty) = unmarshallType (concat["(", fr, ").", name], concat[n, "_", name], spec)
		in
		  (l, ty ^ "\n")
		end
	  val unmarshalled = mapI mk_unmarshall fields
	  fun proj1 l = map (fn (a,b) => a) l
	  fun proj2 l = map (fn (a,b) => b) l
	  in (
	    List.concat [
		List.concat (proj1 unmarshalled),
		map (fn (name, spec) => concat[ml_val, " ", n, "_", name]) fields
	      ],
	    concat [
		concat (proj2 unmarshalled),
		concat (makeTuple (map (fn (name, _) => n^"_"^name) fields, fn (s) => concat [to," = ",s,";"]))
	      ]
	  ) end
      | unmarshallType (fr,to,t) = fail ["GenerateRuntime.unmarshallType",
					       "unhandled type spec ",IIL.spec_to_string (t)]

    fun output_operation os (a,oper as I.Oper {spec,params,context,...}) = let
      fun on_spec (v1,v2) = (case (spec) of I.TS_Void => v1 | _ => v2 ())
      val n = A.toString a
      val _ = Verbose.message2 [" Processing operation: ",n]
      val in_params = List.filter (fn (I.Prm {dir,...}) =>
				   I.has_In (dir)) params
      val out_params = List.filter (fn (I.Prm {dir,...}) =>
				    I.has_Out (dir)) params
      fun mapName (I.Prm {name,...}) = A.toString name
      val names = map mapName params
      val in_names = map mapName in_params
      val out_names = map mapName out_params
      fun mapDecl (I.Prm {name,spec,...}) = ["  ",mk_declaration (ml_op_stub^(A.toString name),spec),";"]
      fun mapMarshall (I.Prm {name,spec,...}) = let 
	val n = A.toString name
      in
	marshallType (ml_arg_stub^n,ml_op_stub^n,spec) p_empty
      end
      fun mapUnmarshall (I.Prm {name,spec,...}) = let
	val n = A.toString name
      in
	unmarshallType (ml_op_stub^n,ml_arg_stub^n,spec)
      end
      val marshalled = map mapMarshall in_params
      val unmarshalled = (on_spec ([],fn () => [unmarshallType ("ml_opresult","ml_argresult",spec)]))@
			     (map mapUnmarshall out_params)
      fun out_decl os dl = app (out os) (map (fn d => ["  ",d,";"]) dl)
      val results = on_spec ([],fn () => ["ml_argresult"])@(map (fn x => ml_arg_stub^x) out_names)
      fun stripRef (I.TS_Ref (spec)) = spec
	| stripRef _ = Error.error ["Trying to ref-strip a non-ref output parameter"]
      fun mapStrippedOutDecl (I.Prm {name,spec,...}) = 
	["  ",mk_declaration ("out_"^ml_op_stub^(A.toString name),stripRef (spec)),";"]
    in
      out os [ml_val," ", ml_fun_stub,n," (",ml_context," *ctx,", ml_val," v) {"];
      out os ["  ",ml_val," ml_result;"];
      out os ["  ",ml_val," ml_argresult;"];
      app (out os) (map (fn (n) => ["  ",ml_val," ",ml_arg_stub,n,";"]) names);
      on_spec ((), fn () => out os ["  ",mk_declaration ("ml_opresult",spec),";"]);
      app (out os) (map mapDecl in_params);
      app (out os) (map mapDecl out_params);
      (* hack to avoid allocating memory on the heap
       * one way to clean it up is to *not* make [out] parameters
       * automatic refs in the IIL, and deal with out parameters specially
       * in the argument list 
       *)
      app (out os) (map mapStrippedOutDecl out_params);
      app (out_decl os) (map (fn (d,_) => d) marshalled);
      app (out_decl os) (map (fn (d,_) => d) unmarshalled);
      (* second bit of the above mentionned hack *)
      app (out os) (map (fn (n) => [F.format "  %s%s = &out_%s%s;" [F.STR (ml_op_stub),
								   F.STR (n),
								   F.STR (ml_op_stub),
								   F.STR (n)]]) out_names);
      case (in_names)
	of [] => ()
	 | [x] => out os [F.format "  %s%s = v;" [F.STR (ml_arg_stub),F.STR (x)]]
	 | l => (out os ["  ", ml_tuple (length l), "ml_arg_tuple = ", 
			 downcast(ml_tuple (length l), "v"), ";"];
		 appI (fn (x,i) => out os [F.format "  %s%s = ml_arg_tuple.Get(ctx, %d);" 
						    [F.STR (ml_arg_stub),
						     F.STR (x),
						     F.INT (i+1)]]) l);
      app (out os) (map (fn c => ["  ",c]) (List.concat (#2 (ListPair.unzip marshalled))));
      out os ["  ",on_spec ("",fn () => "ml_opresult = "),n," (", 
	      (* pass context if need be *)
	      if (context) then concat ["ctx",
					case (names) of [] => "" | _ => ","]
		else "",
	      concatSep (", ",map (fn (n) => (ml_op_stub^n)) names),");"];
      app (out os) (map (fn (_,c) => ["  ",c]) unmarshalled);
      out os (makeTuple (results,fn (s) => concat ["  return ",s,";"]));
      out os ["}\n"]
    end

    fun generateGlue (file, iil) = let
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

  (*
   * entry point (generate function)
   *)
    fun generate {srcDir, srcFile, dstDir, spec} = let
	  val (I.IIL{symtable, decls,...}) = spec
	  val file_hxx = Util.replace_extension (srcFile, "", "hxx")
	  val proto_file = replace_extension (srcFile, "-lib", "hxx")
	  fun global (os) = let
		fun output_decl (I.Type (name,td)) = ()
		  | output_decl (I.Constant (name,c)) = ()
		  | output_decl (I.Operation (name,oper)) = output_operation os (name,oper)
		  | output_decl _ = ()
		in
		  app output_decl decls
		end
	  fun all os = (
		out os ["#include \"SMLNJ/base.h\""];
		out os ["#include \"SMLNJ/ml-values.hxx\""];
		out os ["#include \"", file_hxx, "\""];
		out os ["#include \"", proto_file, "\""];
		global os) 
	  val file_cxx = replace_extension (srcFile, "", "cxx")
	  in
	    GenerateCXXHeader.generate {
		srcDir=srcDir, srcFile=srcFile, dstDir=dstDir, spec=spec
	      };
	    Verbose.message1 ["Generating C++ code in file ", file_cxx];
	    glob_symtable := SOME (symtable);
	    withOutputFile (FILE_CXX, file_cxx, all);
	    generateGlue (srcFile, spec)
	  end 
	    handle ex => (print(concat["uncaught exception ", exnName ex, "\n"]); raise ex)

    val _ = CmdOptions.register ("new-rt", generate)

  end
