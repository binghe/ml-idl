(* generate-runtime.sml
 *
 * CHANGES:
 * 6/18/01 - moved the typedef and cpp_quote stuff to the header
 *)

(* add enumeration types *)

structure GenerateClassicRuntime : sig end = struct

  structure I = IIL
  structure A = Atom
  structure S = AtomMap
  structure F = Format
    
  open Util

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  val mapP = List.mapPartial
    
  (* for customization *)
  val ml_val = "ml_val_t"
  val ml_context = "ml_state_t"
  val ml_fun_stub = "ml_stub_"
  val ml_arg_stub = "ml_arg_"
  val ml_op_stub = "ml_op_"

  fun ml_sub (i,v) = concat ["REC_SEL (",v,",",Int.toString (i),")"]

  local 
    val i = ref (0)
  in
    fun gensym () = "ml_tmp_"^(Int.toString (!i)) before i := (!i) + 1
  end
    
  val glob_symtable = ref (NONE) : I.typedef S.map option ref

  fun findType (I.TS_Id (s)) = 
        (case (!glob_symtable)
           of NONE => fail ["GenerateRuntime.findType",
                                  "uninitialized global symbol table"]
            | SOME (st) => 
                (case (S.find (st,s))
                   of SOME (I.TypeDef {spec,...}) => findType (spec)
                    | NONE => Error.error ["is not a type"]))
    | findType (t) = t

  (* 
   * Given a name and a type, create a C declaration for that name (minus the ;)
   *)
  fun mk_declaration (n,I.TS_Id (s)) = concat [A.toString (s)," ",n]
    | mk_declaration (n,I.TS_Real64) = concat ["double ",n]
    | mk_declaration (n,I.TS_Real32) = concat ["double ",n]
    | mk_declaration (n,I.TS_Int32) = concat ["Int32_t ",n]
    | mk_declaration (n,I.TS_Int16) = concat ["Int32_t ",n]
    | mk_declaration (n,I.TS_Int8) = concat ["Int32_t ",n]
    | mk_declaration (n,I.TS_Word32) = concat ["Word_t ",n]
    | mk_declaration (n,I.TS_Word16) = concat ["Word_t ",n]
    | mk_declaration (n,I.TS_Word8) = concat ["Word_t ",n]
    | mk_declaration (n,I.TS_Int) = concat ["int ",n]
    | mk_declaration (n,I.TS_Word) = concat ["unsigned int ",n]
    | mk_declaration (n,I.TS_Bool) = concat ["bool_t ",n]
    | mk_declaration (n,I.TS_Char) = concat ["char ",n]
    | mk_declaration (n,I.TS_String) = concat ["char *",n]
    | mk_declaration (n,I.TS_Ref spec) = mk_declaration ("*"^n,spec)
    | mk_declaration (n,I.TS_Ptr spec) = mk_declaration ("*"^n,spec)
    | mk_declaration (n,I.TS_Option (I.TS_String)) = concat ["char *",n]
    | mk_declaration (n,I.TS_Option (I.TS_Ref spec)) = mk_declaration ("*"^n,spec)
    | mk_declaration (n,I.TS_Array {spec,size}) = 
        (case (size) 
           of I.E_Int (i) => mk_declaration (concat [n,"[",Int32.toString (i),"]"],spec)
            | _ => mk_declaration ("*"^n,spec))
    | mk_declaration (n,I.TS_StructTag (a)) = concat ["struct ",A.toString (a)," ",n]
    | mk_declaration (n,I.TS_Sml _) = concat [ml_val," ",n]
    | mk_declaration (n,I.TS_App {oper,...}) = mk_declaration (n,oper)
    | mk_declaration (n,I.TS_Dep {spec,...}) = mk_declaration (n,spec)
    | mk_declaration (n,ts) = fail ["GenerateRuntime.mk_declaration",
                                          "unhandled type spec ",IIL.spec_to_string (ts)]

    type rho = (A.atom * I.exp) list
    val p_empty = [] : rho
    fun p_lookup ([],a) = Error.error ["Unbound variable ",A.toString a]
      | p_lookup (((x,y)::xs),a) = if (A.sameAtom (x,a))
                                     then y
                                   else p_lookup (xs,a)
    fun p_add (p,(x,y)) = (x,y)::p

    fun str_constant (p,I.E_Int (i)) = Int32.toString (i)
      | str_constant (p,I.E_Id (x)) = str_constant (p,p_lookup (p,x))  (* assume alpha-renaming all the way through *)
      | str_constant (p,I.E_String (x)) = x

    fun makeTuple (results:string list,what:string -> string):string list = 
      case (results)
        of [] => [what ("ML_unit")]
         | [x] => [what (x)]
         | l => ["  {\n",
                 "    ml_val_t    *__p = msp->ml_allocPtr;\n",
                 "    ml_val_t    r;\n",
                 concat ["    *__p++ = MAKE_DESC(",Int.toString (length (l)),",DTAG_record);\n"],
                 concat (map (fn (v) => concat ["    *__p++ = ",v,";\n"]) l),
                 "    r = PTR_CtoML(msp->ml_allocPtr + 1);\n",
                 "    msp->ml_allocPtr = __p;\n",
                 "  ",what ("r"),
                 "  };"]

  (* 
   * Given two variables and a type, create the code that marshalls 
   * The result is a pair (dl,code) with 'dl' a list of declarations used as temporary variables
   * and code the actual code to perform the marshalling.
   *)
  fun marshallType (fr,to,I.TS_Id (s)) p = marshallType (fr,to,findType (I.TS_Id (s))) p
      (* ([],concat [to," = marshall_",A.toString s," (msp,",fr,");"]) *)
    | marshallType (fr,to,I.TS_Real64) p = ([],concat [to," = *(PTR_MLtoC (double,",fr,"));"])
    | marshallType (fr,to,I.TS_Real32) p = ([],concat [to," = *(PTR_MLtoC (double,",fr,"));"])
    | marshallType (fr,to,I.TS_Int32) p = ([],concat [to," = INT32_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Int16) p = ([],concat [to," = INT32_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Int8) p = ([],concat [to," = INT32_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Word32) p = ([],concat [to," = WORD_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Word16) p = ([],concat [to," = WORD_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Word8) p = ([],concat [to," = WORD_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Int) p = ([],concat [to," = INT_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Word) p = ([],concat [to," = INT_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Bool) p = ([],concat [to," = INT_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Char) p = ([],concat [to," = (char) INT_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_String) p = ([],concat [to," = STR_MLtoC (",fr,");"])
    | marshallType (fr,to,I.TS_Option (I.TS_String)) p = let
        val n = gensym ()
        val (d,c) = marshallType (n,to,I.TS_String) p
      in
        (d,concat ["if (",fr," == OPTION_NONE) { ",to," = NULL; } else { ",ml_val," ",n,"; ",n," = OPTION_get (",fr,"); ", c," };"])
      end
    | marshallType (fr,to,I.TS_Ptr spec) p = (*([],concat [to," = PTR_MLtoC (void,",fr,");"]) *)
        ([],concat [to," = PTR_MLtoC (",I.spec_to_string (spec),",",fr,");"])
    | marshallType (fr,to,I.TS_Option (I.TS_Ref spec)) p = let
        val n = gensym ()
        val n_opt = gensym ()
        val (d,c) = marshallType (n_opt,n,spec) p
        val d' = mk_declaration (n,spec)
      in 
        (d'::d,concat ["if (",fr," == OPTION_NONE) { ",to," = NULL; } else { ",ml_val," ",n_opt,"; ",n_opt," = OPTION_get (",fr,"); ",c," ",to," = &",n,";};"])
      end
    | marshallType (fr,to,I.TS_Ref spec) p = let
        val n = gensym () 
        val (d,c) = marshallType (fr,n,spec) p
        val d' = mk_declaration (n,spec)
      in
        (d'::d,concat [c," ",to," = &",n,";"]) 
      end
    | marshallType (fr,to,I.TS_Array {spec,size}) p = let
        val (d,c) = marshallType ("<sub i from "^fr^">",to^"[i]",spec) p
        val size_bnd = str_constant (p,size)
        val alloc = case (size)
                      of I.E_Int _ => ""
                       | _ => concat [to," = (<cast>) malloc (<size> * ",size_bnd,"); "]
      in
        (d,concat [alloc,"for (int i=0; i < ",size_bnd,"; i++) ",c])
      end
    | marshallType (fr,to,I.TS_Sml _) p = ([],concat [to," = ",fr, ";"])
    | marshallType (fr,to,I.TS_App {oper=I.TS_Dep {id,id_spec,spec},app}) p = let
        val app' = case (app) 
                     of I.E_Id (var) => I.E_String (ml_op_stub^(A.toString (var)))
                      | _ => fail ["GenerateRuntime.marshallType","Huh? not a E_Id..."]
      in
        marshallType (fr,to,spec) (p_add (p,(id,app')))
      end
    | marshallType (fr,to,I.TS_Struct (fields)) p = let
        val fields = map (fn (I.Fld {spec,name,...}) => (A.toString name,spec)) fields
        fun mk_marshall ((name,spec),i) = marshallType (ml_sub(i,fr),"("^to^")."^name,spec) p_empty
        val marshalled = mapI mk_marshall fields
        fun proj1 l = map (fn (a,b) => a) l
        fun proj2 l = map (fn (a,b) => b) l
      in
        (List.concat (proj1 marshalled),
         concat (proj2 marshalled))
      end
    | marshallType (fr,to,t) p = fail ["GenerateRuntime.marshallType",
                                           "unhandled type spec ",IIL.spec_to_string (t)]
      
  fun unmarshallType (fr,to,I.TS_Id (s)) = unmarshallType (fr,to,findType (I.TS_Id (s)))
      (* ([],concat [to," = unmarshall_",A.toString s," (msp,",fr,");"]) *)
    | unmarshallType (fr,to,I.TS_Real64) = ([],concat ["REAL64_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Real32) = ([],concat ["REAL64_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Int32) = ([],concat ["INT32_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Int16) = ([],concat ["INT32_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Int8) = ([],concat ["INT32_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Word32) = ([],concat ["WORD_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Word16) = ([],concat ["WORD_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Word8) = ([],concat ["WORD_ALLOC (msp,",to,",",fr,");"])
    | unmarshallType (fr,to,I.TS_Int) = ([],concat [to," = INT_CtoML (",fr,");"])
    | unmarshallType (fr,to,I.TS_Word) = ([],concat [to," = INT_CtoML (",fr,");"])
    | unmarshallType (fr,to,I.TS_Bool) = ([],concat [to," = INT_CtoML (",fr,");"])
    | unmarshallType (fr,to,I.TS_Char) = ([],concat [to," = INT_CtoML ((int) ",fr,");"])
    | unmarshallType (fr,to,I.TS_String) = ([],concat [to," = ML_AllocString (msp,strlen (",fr,"));\n  strcpy (STR_MLtoC (",to,"),",fr,"); /* get length right */"])
    | unmarshallType (fr,to,I.TS_Option (I.TS_String)) = ([],concat [to," = <to_ml_opt_string> (",fr,");"])
    | unmarshallType (fr,to,I.TS_Ptr spec) = ([],concat [to," = PTR_CtoML (",fr,");"])
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
    | unmarshallType (fr,to,I.TS_Sml _) = ([],concat [to," = ",fr, ";"])
    | unmarshallType (fr,to,I.TS_Struct (fields)) = let
        val n = gensym ()
        val fields = map (fn (I.Fld {spec,name,...}) => (A.toString name,spec)) fields
        fun mk_unmarshall ((name,spec),i) = let
	      val (l, ty) = unmarshallType ("("^fr^")."^name,n^"_"^name,spec)
	      in
		(l, ty ^ "\n")
	      end
        val unmarshalled = mapI mk_unmarshall fields
        fun proj1 l = map (fn (a,b) => a) l
        fun proj2 l = map (fn (a,b) => b) l
      in
        (List.concat [List.concat (proj1 unmarshalled),
                      map (fn (name,spec) => ml_val^" "^n^"_"^name) fields],
         concat [concat (proj2 unmarshalled),
                 concat (makeTuple (map (fn (name,_) => n^"_"^name) fields,fn (s) => concat [to," = ",s,";"]))])
      end
    | unmarshallType (fr,to,t) = fail ["GenerateRuntime.unmarshallType",
                                             "unhandled type spec ",IIL.spec_to_string (t)]

(*  fun freeType (fr,t) = "/* <free "^(IIL.spec_to_string t)^" from "^fr^">; */" *)

  fun output_marshall os (n,f) = 
    (out os [n," marshall_",n," (",ml_context," *msp,", ml_val," v) {"];
     f ();
     out os ["}"])

  fun output_unmarshall os (n,f) = 
    (out os [ml_val," unmarshall_",n," (",ml_context," *msp, ",n," v) {"];
     f ();
     out os ["}"])

(*  fun output_free os (n,f) = 
    (out os ["void free_",n," (",n," v) {"];
     f ();
     out os ["}"])
*)

  fun output_struct os (a,fields) = let
    val m_n = gensym  ()
    val u_n = gensym  ()
    val n = A.toString a
    val fields = map (fn (I.Fld {spec,name,...}) => (A.toString name,spec)) fields
    fun out_field os (name,spec) = out os ["  ",mk_declaration (name,spec),";"]
    fun mk_marshall ((name,spec),i) = marshallType (ml_sub(i,"v"),m_n^"."^name,spec) p_empty
    fun mk_unmarshall ((name,spec),i) = unmarshallType ("v."^name,name,spec)
    val marshalled = mapI mk_marshall fields
    val unmarshalled = mapI mk_unmarshall fields
    fun out_marshall os (d,c) = out os ["  ",c]
    fun marshall () = (out os ["  ",n," ",m_n,";"];
                       app (out_marshall os) marshalled;
                       out os ["  return ",m_n,";"])
    fun out_decl os (name,spec) = out os ["  ",ml_val," ",name,";"]
    fun out_unmarshall os (d,c) = out os ["  ",c]
    fun unmarshall () = (out os ["  ",ml_val," ",u_n,";"];
                         app (out_decl os) fields;
                         app (out_unmarshall os) unmarshalled;
                         out os ["  ",u_n," = <make tuple from ",
                                 concatSep (", ",map (fn (name,_) => name) fields),">"];
                         out os ["  return ",u_n])
(*    fun free () = () *)
  in
    out os ["typedef struct {"];
    app (out_field os) fields;
    out os ["} ",n,";"];
(* THESE HAVE NOW BEEN INLINED *)
(*    output_marshall os (n,marshall); *)
(*    output_unmarshall os (n,unmarshall); *)
(*    output_free os (n,free); *)
    out os []
  end

  fun output_def os (a,spec) = let
    val m_n = gensym  ()
    val u_n = gensym  ()
    val n = A.toString a
    val s = mk_declaration (n,spec)
    val (m_dl,m_c) = marshallType ("v",m_n,spec) p_empty
    val (u_dl,u_c) = unmarshallType ("v",u_n,spec)
    fun out_decl os dl = app (out os) (map (fn d => ["  ",d,";"]) dl)
    fun marshall () = (out_decl os m_dl;
                       out os ["  ",mk_declaration (m_n,spec),";"];
                       out os ["  ",m_c];
                       out os ["  return ",m_n,";"])
    fun unmarshall () = (out_decl os u_dl;
                         out os ["  ",ml_val," ",u_n,";"];
                         out os ["  ",u_c];
                         out os ["  return ",u_n,";"])
(*    fun free () = out os ["  ",freeType ("v",spec)] *)
  in
    out os ["typedef ",s,";"];
(*    output_marshall os (n,marshall); *)
(*    output_unmarshall os (n,unmarshall); *)
(*    output_free os (n,free); *)
    out os []
  end

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
(*    fun mapFree (I.Prm {name,spec,...}) = let
      val n = A.toString name
    in
      ["  ",freeType (ml_op_stub^n,spec)]
    end *)
    fun out_decl os dl = app (out os) (map (fn d => ["  ",d,";"]) dl)
    val results = on_spec ([],fn () => ["ml_argresult"])@(map (fn x => ml_arg_stub^x) out_names)
    fun stripRef (I.TS_Ref (spec)) = spec
      | stripRef _ = Error.error ["Trying to ref-strip a non-ref output parameter"]
    fun mapStrippedOutDecl (I.Prm {name,spec,...}) = 
      ["  ",mk_declaration ("out_"^ml_op_stub^(A.toString name),stripRef (spec)),";"]
  in
    out os [ml_val," ", ml_fun_stub,n," (",ml_context," *msp,", ml_val," v) {"];
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
       | l => appI (fn (x,i) => out os [F.format "  %s%s = REC_SEL (v,%d);" [F.STR (ml_arg_stub),
                                                                             F.STR (x),
                                                                             F.INT (i)]]) l;
    app (out os) (map (fn (_,c) => ["  ",c]) marshalled);
    out os ["  ",on_spec ("",fn () => "ml_opresult = "),n," (", 
            (* pass context if need be *)
            if (context) then concat ["msp",
                                      case (names) of [] => "" | _ => ","]
              else "",
            concatSep (", ",map (fn (n) => (ml_op_stub^n)) names),");"];
    app (out os) (map (fn (_,c) => ["  ",c]) unmarshalled);
    out os (makeTuple (results,fn (s) => concat ["  return ",s,";"]));
    out os ["}\n"]
  end

(* now in header file
 *
  fun output_type_def os (a,I.TypeDef {spec,exclude,abstract,...}) = let
    val n = A.toString (a)
    val _ = Verbose.message2 [" Processing type definition: ",n]
    fun output_type os (a,I.TS_Struct (fields)) = 
          if abstract
            then let
              val foo = 10
            in
              out os ["typedef struct <sometag!> foo"]
            end
          else output_struct os (a,fields)
      | output_type os (a,spec) = output_def os (a,spec)
  in
    if (exclude)
      then ()
    else output_type os (a,spec)
  end
*)

(* Now in header file
 *
  fun output_const os (a,I.Const {value,spec,...}) = let
    val (t,v) = case (value)
                  of I.E_Int (i) => ("int ",Int32.toString i)
                   | I.E_Bool (b) => ("int ",if (b) then "TRUE" else "FALSE")
                   | I.E_String (s) => ("char *",concat ["\"",s,"\""])
                   | I.E_Char (c) => ("char ",concat ["'",Char.toString c,"'"])
  in
    out os [t,A.toString a," = ", v,";\n"]
  end

*)

  (*
   * entry point (generate function)
   *)
  fun generate {srcDir, srcFile, dstDir, spec} = let
	val (I.IIL {symtable, decls,...}) = spec
        val file_h = Util.replace_extension (srcFile, "", "h")
	fun global (os) = let
	      fun output_decl (I.Type (name,td)) = ()
        	| output_decl (I.Constant (name,c)) = ()
        	| output_decl (I.Operation (name,oper)) = output_operation os (name,oper)
                | output_decl _ = ()
	      in
		app output_decl decls
	      end
	fun all os = (out os ["#include \"ml-base.h\""];
                      out os ["#include \"ml-values.h\""];
                      out os ["#include \"ml-objects.h\"\n"];
                      out os ["#include \"",file_h,"\"\n"];
                      global os) 
	val file_c = replace_extension (srcFile,"","c")
	in
          GenerateHeader.generate {
	      srcDir=srcDir, srcFile=srcFile, dstDir=dstDir, spec=spec, cxx=false
	    };
	  Verbose.message1 ["Generating C code in file ",file_c];
	  glob_symtable := SOME (symtable);
	  withOutputFile (FILE_C, file_c,all);
	  GenerateClassicRuntimeGlue.generate (srcFile, spec)
	end 
handle ex => (print(concat["uncaught exception ", exnName ex, "\n"]); raise ex)

  val _ = CmdOptions.register ("classic", generate)

end
