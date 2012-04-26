(* generate-runtime.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module generates the C++ code to package up a library for
 * the C++ version of the runtime system.
 *)


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

  (* representation of C++ types *)
    datatype cxx_ty
      = T_Named of string
      | T_Ptr of cxx_ty			(* ty * *)
      | T_Ref of cxx_ty			(* ty & *)
      | T_Array of cxx_ty * int option
      | T_App of string * cxx_ty list	(* type application *)

    fun cxxTyToString ty = (case ty
	   of T_Named ty => ty
	    | T_Ptr ty => cxxTyToString ty ^ " *"
	    | T_Ref ty => cxxTyToString ty ^ " &"
	    | T_Array(ty, NONE) => cxxTyToString ty ^ "[]"
	    | T_Array(ty, SOME d) => concat[cxxTyToString ty, "[", Int.toString d, "]"]
	    | T_App(ty, []) => ty ^ "<>"
	    | T_App(ty, args) => let
		fun f [ty] = let
		      val ty = cxxTyToString ty
		      in
			if (String.sub(ty, String.size ty - 1) = #">")
			  then [ty, " >"]
			  else [ty, ">"]
		      end
		  | f (ty::tys) = cxxTyToString ty :: "," :: f tys
		in
		  concat(ty :: "<" :: f args)
		end
	  (* end case *))

    fun varDecl (ty, n) = (case ty
	   of T_Named ty => concat[ty, " ", n]
	    | T_Ptr ty => varDecl (ty, "*"^n)
	    | T_Ref ty => varDecl (ty, "&"^n)
	    | T_Array(ty, NONE) => varDecl (ty, n^"[]")
	    | T_Array(ty, SOME d) => varDecl (ty, concat[n, "[", Int.toString d, "]"])
	    | T_App _ => concat[cxxTyToString ty, " ", n]
	  (* end case *))

    val mapP = List.mapPartial

  (* the length of the longest specialized ML_TupleN type (see ml-values.hxx in runtime system) *)
    val maxFixedTuple = 5

  (* for customization *)
    val ml_val = "ML_Value"
    val ml_context = "ML_Context"
    val ml_fun_stub = "ml_stub_"
    val ml_out_stub = "ml_out_"
    val ml_in_stub = "ml_in_"

    fun ml_tuple n = "ML_Tuple<" ^ (Int.toString n) ^ ">"

    local 
      val i = ref 0
    in
    fun gensym () = ("ml_tmp_" ^ Int.toString (!i)) before i := !i + 1
    end
    
    val glob_symtable = ref (NONE : I.typedef S.map option)

    fun findType s = (case (!glob_symtable)
	   of NONE => fail ["GenerateRuntime.findType uninitialized global symbol table"]
	    | SOME st => (case S.find (st, s)
		 of SOME(I.TypeDef{spec = I.TS_Id s, ...}) => findType s
		  | SOME(I.TypeDef{spec, ...}) => spec
		  | NONE => Error.error ["is not a type"]
		(* end case *))
	  (* end case *))

  (* map an IDL type to the C++ encoding of its ML type *)
    fun idlToMLType ty = (case ty
	   of I.TS_Id s => idlToMLType(findType s)
	    | I.TS_Real64 => T_Named "ML_Real64"
	    | I.TS_Real32 => T_Named "ML_Real64"
	    | I.TS_Int32 => T_Named "ML_Int32"
	    | I.TS_Int16 => T_Named "ML_Int"
	    | I.TS_Int8 => T_Named "ML_Int"
	    | I.TS_Int => T_Named "ML_Int"
	    | I.TS_Word32 => T_Named "ML_Word32"
	    | I.TS_Word16 => T_Named "ML_Word"
	    | I.TS_Word8 => T_Named "ML_Word"
	    | I.TS_Word => T_Named "ML_Word"
	    | I.TS_Bool => T_Named "ML_Int"
	    | I.TS_Char => T_Named "ML_Int"
	    | I.TS_String => T_Named "ML_String"
	    | I.TS_Option(I.TS_Ref spec) => T_App("ML_Option", [idlToMLType spec])
	    | I.TS_Ref spec => idlToMLType spec
	    | I.TS_Sml(_, NONE) => T_Named "ML_Value"
	    | I.TS_Sml(_, SOME ty) => T_Named ty
(*
	    | I.TS_App{oper=I.TS_Dep{id,id_spec,spec},app} => ??
*)
	    | I.TS_Struct fields => (* map to tuple type *)
		idlToTupleTy (List.map (fn (I.Fld{spec, ...}) => spec) fields)
	    | t => fail [
		  "idlToMLType.idlToMLType", "unhandled type spec ", IIL.spec_to_string t
		]
	  (* end case *))

    and idlToTupleTy [] = T_Named "ML_Value"	(* really "unit" *)
      | idlToTupleTy [ty] = idlToMLType ty
      | idlToTupleTy specs = let
	  val n = List.length specs
	  in
	    if (n > maxFixedTuple)
	      then T_Named (concat["ML_Tuple<", Int.toString n, ">"])
	      else T_App("ML_Tuple" ^ Int.toString n, List.map idlToMLType specs)
	  end

  (* convert an IDL type to its C/C++ representation *)
    fun idlToCXXType ty = (case ty
	   of I.TS_Id s => T_Named(Atom.toString s)
	    | I.TS_Real64 => T_Named "double "
	    | I.TS_Real32 => T_Named "float "
	    | I.TS_Int32 => T_Named "int32_t "
	    | I.TS_Int16 => T_Named "int32_t "
	    | I.TS_Int8 => T_Named "int32_t "
	    | I.TS_Word32 => T_Named "uint32_t "
	    | I.TS_Word16 => T_Named "uint32_t "
	    | I.TS_Word8 => T_Named "uint32_t "
	    | I.TS_Int => T_Named "int "
	    | I.TS_Word => T_Named "unsigned int "
	    | I.TS_Bool => T_Named "bool "
	    | I.TS_Char => T_Named "char "
	    | I.TS_String => T_Ptr(T_Named "const char")
	    | I.TS_Ref spec => T_Ptr(idlToCXXType spec)
	    | I.TS_Ptr spec => T_Ptr(idlToCXXType spec)
	    | I.TS_Option I.TS_String => T_Ptr(T_Named "const char")
	    | I.TS_Option(I.TS_Ref spec) => T_Ptr(idlToCXXType spec)
	    | I.TS_StructTag a => T_Named(Atom.toString a)
	    | I.TS_Sml(_, NONE) => T_Named "ML_Value"
	    | I.TS_Sml(_, SOME ty) => T_Named ty
	    | I.TS_App{oper, ...} => idlToCXXType oper
	    | I.TS_Dep{spec, ...} => idlToCXXType spec
	    | ts => fail [
		 "GenerateRuntime.idlToCXXType", "unhandled type spec ", IIL.spec_to_string ts
	       ]
	  (* end case *))

  (* marshall data from ML representation to native C++ representation.  The source of the
   * data is assumed to be a C++ object whose type is defined by idlToTupleTy.  The variable
   * ml_param is the name of the source object.  The destination is specified as a list of
   * (idl-type, name) pairs.
   *)
    fun marshallParams specs = let
	  fun mapi f l = let
		fun mapf (i, []) = []
		  | mapf (i, x::xs) = f(i, x) :: mapf(i+1, xs)
		in
		  mapf (0, l)
		end
	  fun marshallParam (isTop, optTyName, pSpec, pName, rhs) = let
		fun tyName ty = (case optTyName of NONE => ty | SOME ty' => Atom.toString ty')
		fun initScalar (ty, name) = let
		      val asgn = [pName, " = ", rhs, ".Val();"]
		      in
			if isTop
			  then "  " :: tyName ty :: " " :: asgn
			  else "  " :: asgn
		      end
		in
		  case pSpec
		   of I.TS_Id s => marshallParam (isTop, SOME s, findType s, pName, rhs)
		    | I.TS_Real64 => [initScalar("double", pName)]
		    | I.TS_Real32 => [initScalar("float", pName)]
		    | I.TS_Int32 => [initScalar("int32_t", pName)]
		    | I.TS_Int16 => [initScalar("int16_t", pName)]
		    | I.TS_Int8 => [initScalar("int8_t", pName)]
		    | I.TS_Int => [initScalar("int", pName)]
		    | I.TS_Word32 => [initScalar("uint32_t", pName)]
		    | I.TS_Word16 => [initScalar("uint16_t", pName)]
		    | I.TS_Word8 => [initScalar("uint8_t", pName)]
		    | I.TS_Word => [initScalar("unsigned int", pName)]
		    | I.TS_Bool => [initScalar("bool", pName)]
		    | I.TS_Char => [initScalar("char", pName)]
		    | I.TS_String => if isTop
			then [["  const char *", pName, " = ", rhs, ".UnsafeCString();"]]
			else [["  ", pName, " = ", rhs, ".UnsafeCString();"]]
		    | I.TS_Option(I.TS_Ref spec) => [
(* FIXME: what if isTop = false? *)
			  ["  ", varDecl(idlToCXXType pSpec, pName), ";"],
			  ["  if (", rhs, ".isNONE()) {"],
			  ["    ", pName, " = 0;"],
			  ["  } else {"],
			  ["/*** FIXME ***/"],
			  ["  }"]
			]
		    | I.TS_Ref spec => marshallParam (isTop, optTyName, spec, pName, rhs)
		    | I.TS_Sml(_, NONE) => if isTop
			then [["  ML_Value ", pName, " = ", rhs, ";"]]
			else raise Fail "unexpected nested SML spec"
		    | I.TS_Sml(_, SOME ty) => if isTop
			then [["  ", ty, " ", pName, " = ", rhs, ";"]]
			else raise Fail "unexpected nested SML spec"
(*
		    | I.TS_App{oper=I.TS_Dep{id,id_spec,spec},app} => ??
*)
		    | I.TS_Struct fields => let
			val len = List.length fields
			val fields = if len > maxFixedTuple
			      then List.concat (mapi (marshallField' (pName, rhs)) fields)
			      else List.concat (mapi (marshallField (pName, rhs)) fields)
			in
			  if isTop
			    then let
			      val dcl = (case optTyName
				      of NONE => ["  ", varDecl(idlToCXXType pSpec, pName), ";"]
				       | SOME ty => ["  ", Atom.toString ty, " ", pName, ";"]
				    (* end case *))
			      in
				dcl :: fields
			      end
			    else fields
			end
		    | t => fail [
			  "GenerateRuntime.marshallParams", "unhandled type spec ", IIL.spec_to_string t
			]
		  (* end case *)
		end
	(* marshall struct fields, where the C++ representation has precise type information *)
	  and marshallField (lhs, rhs) (i, (I.Fld{spec, name})) = marshallParam (
		false, NONE, spec,
		concat[lhs, ".", Atom.toString name],
		concat[rhs, ".Get", Int.toString(i+1), "(ctx)"])
	(* marshall struct fields, where the C++ representation is ML_Tuple<N> *)
	  and marshallField' (lhs, rhs) (i, (I.Fld{spec, name})) = let
	      (* C++ type that represents the ML type of the field *)
		val mlTy = cxxTyToString(idlToMLType spec)
		in
		  marshallParam (
		    false, NONE, spec,
		    concat[lhs, ".", Atom.toString name],
		    concat[mlTy, "(", rhs, ".Get(ctx, ", Int.toString(i+1), "))"])
		end
	  in
	    case specs
	     of [] => []
	      | [(pSpec, pName)] => marshallParam (true, NONE, pSpec, ml_in_stub ^ pName, "ml_param")
	      | _ => let
		  fun marshallElem (i, (pSpec, pName)) = marshallParam (
			true, NONE, pSpec, ml_in_stub ^ pName,
			concat["ml_param.Get", Int.toString(i+1), "(ctx)"])
		  fun marshallElem' (i, (pSpec, pName)) = let
		      (* C++ type that represents the ML type of the field *)
			val mlTy = cxxTyToString(idlToMLType pSpec)
			in
			  marshallParam (
			    true, NONE, pSpec, ml_in_stub ^ pName,
			    concat[mlTy, "(ml_param.Get(ctx, ", Int.toString(i+1), "))"])
			end
		  val len = List.length specs
		  in
		    if len > maxFixedTuple
		      then List.concat (mapi marshallElem' specs)
		      else List.concat (mapi marshallElem specs)
		  end
	    (* end case *)
	  end

  (* unmarshall results *)
    fun unmarshallResults specs = let
	  val stms = ref[];
	  fun unmarshallResult (pSpec, pName) = let
		fun unboxed ty = concat[ty, "(", pName, ")"]
		fun boxed ty = concat[ty, "(ctx, ", pName, ")"]
		in
		  case pSpec
		   of I.TS_Id s => unmarshallResult (findType s, pName)
		    | I.TS_Real64 => boxed "ML_Real64"
		    | I.TS_Real32 => boxed "ML_Real64"
		    | I.TS_Int32 => boxed "ML_Int32"
		    | I.TS_Int16 => unboxed "ML_Int"
		    | I.TS_Int8 => unboxed "ML_Int"
		    | I.TS_Int => unboxed "ML_Int"
		    | I.TS_Word32 => boxed "ML_Word32"
		    | I.TS_Word16 => unboxed "ML_Word"
		    | I.TS_Word8 => unboxed "ML_Word"
		    | I.TS_Word => unboxed "ML_Word"
		    | I.TS_Bool => unboxed "ML_Bool"
		    | I.TS_Char => unboxed "ML_Char"
		    | I.TS_String => boxed "ML_String"
		    | I.TS_Option(I.TS_Ref spec) => "" (* FIXME *)
		    | I.TS_Ref spec => unmarshallResult (spec, pName) (* ?? *)
		    | I.TS_Sml _ => pName
(*
		    | I.TS_App{oper=I.TS_Dep{id,id_spec,spec},app} => ??
*)
		    | I.TS_Struct fields => let
			val len = List.length fields
			fun unmarshallField (I.Fld{spec, name}) =
			      unmarshallResult (spec, concat[pName, ".", Atom.toString name])
			val fields = List.map unmarshallField fields
			in
			  if len > maxFixedTuple
			    then let
			      val mlTy = concat["ML_Tuple<", Int.toString len, ">"]
			      val tmp = gensym ()
			      val stm = [
				      "  ", ml_val, " ", tmp, "[", Int.toString len, "] = {",
				      concatSep(", ", fields), "};"
				    ]
			      in
				stms := stm :: !stms;
				concat[mlTy, "(ctx, ", tmp, ")"]
			      end
			    else let
			      val mlTy = cxxTyToString(idlToMLType pSpec)
			      in
				concat[mlTy, "(", concatSep(", ", "ctx" :: fields), ")"] 
			      end
			end
		    | t => fail [
			  "GenerateRuntime.unmarshallResults", "unhandled type spec ", IIL.spec_to_string t
			]
		  (* end case *)
		end
	  val stm = (case specs
		 of [] => ["  ML_Value result();"]
		  | [(rSpec, rName)] =>
		      ["  ", varDecl(idlToMLType rSpec, "result"), " = ", unmarshallResult (rSpec, rName), ";"]
		  | _ => let
		      val len = List.length specs
		      val elems = concatSep(", ", List.map unmarshallResult specs)
		      in
			if len > maxFixedTuple
			  then let
			    val mlTy = concat["ML_Tuple<", Int.toString len, ">"]
			    val tmp = gensym ()
			    val stm = [
				    "  ", ml_val, " ", tmp, "[", Int.toString len, "] = {",
				    elems, "};"
				  ]
			    in
			      stms := stm :: !stms;
			      ["  ", mlTy, " result(ctx, ", tmp, ");"]
			    end
			  else let
			    val mlTy = cxxTyToString(idlToTupleTy (List.map #1 specs))
			    in
			      ["  ", mlTy, " result(ctx, ", elems, ");"] 
			    end
		      end
		(* end case *))
	  in
	    List.rev(stm :: !stms)
	  end

  (* generate code for the wrapper of an operation in the IDL specification. *)
    fun output_operation os (a, oper as I.Oper{spec, params, context, ...}) = let
	  val isVoid = (case spec of I.TS_Void => true | _ => false)
	  val n = A.toString a
	  val _ = Verbose.message2 [" Processing operation: ", n]
	(* filter out input and output parameters *)
	  val inParams = List.filter (fn (I.Prm{dir,...}) => I.has_In dir) params
	  val outParams = List.filter (fn (I.Prm{dir,...}) => I.has_Out dir) params
	(* the C++ type that matches the tuple of parameters *)
	  val paramTy = idlToTupleTy (List.map (fn (I.Prm{spec, ...}) => spec) inParams)
	  fun prmToString (I.Prm{name,...}) = A.toString name
	  val names = map prmToString params
	  val in_names = map prmToString inParams
	  val out_names = map prmToString outParams
	  in
	    out os [ml_val," ", ml_fun_stub, n, " (", ml_context, " *ctx, ", ml_val, " v) {"];
	    if List.null inParams
	      then ()
	      else (
		out os ["  ", cxxTyToString paramTy, " ml_param(v);"];
		out os ["// marshall arguments"];
		List.app (out os)
		  (marshallParams
		    (List.map (fn (I.Prm{spec, name, ...}) => (spec, Atom.toString name))
		      inParams)));
	  (* output variable declarations *)
	    if List.null outParams
	      then ()
	      else (
		out os ["// output parameters"];
		List.app (out os)
		  (List.map
		    (fn (I.Prm{spec=I.TS_Ref spec, name, ...}) => [
			  "  ", varDecl(idlToCXXType spec, ml_out_stub ^ Atom.toString name), ";"
			])
		      outParams));
	  (* call *)
	    out os ["// call operation"];
	    let
	    fun cvtParam (I.Prm{dir=I.In, spec=I.TS_Ref _, name, ...}) =
		  concat ["&", ml_in_stub, Atom.toString name]
	      | cvtParam (I.Prm{dir=I.In, name, ...}) = ml_in_stub ^ Atom.toString name
	      | cvtParam (I.Prm{dir=I.InOut, spec=I.TS_Ref _, name, ...}) =
		  concat ["&", ml_in_stub, Atom.toString name]
	      | cvtParam (I.Prm{dir=I.Out, spec=I.TS_Ref _, name, ...}) =
		  concat ["&", ml_out_stub, Atom.toString name]
	    val args = List.map cvtParam params
	  (* pass context if need be *)
	    val args = if context then "ctx" :: args else args
	    in
	      out os [
		  "  ",
		  if isVoid then "" else varDecl (idlToCXXType spec, "ml_opresult") ^ " = ",
		  n, " (", concatSep (", ", args), ");"
		]
	    end;
	    let
	    fun cvtResult (I.Prm{dir=I.Out, spec, name, ...}) = (spec, ml_out_stub ^ Atom.toString name)
	      | cvtResult (I.Prm{spec, name, ...}) = (spec, ml_in_stub ^ Atom.toString name)
	    val results = List.map cvtResult outParams
	    val results = if isVoid then results else (spec, "ml_opresult") :: results
	    in
	      if List.null results
		then out os ["  return ML_Tuple<0>();"]
		else (
		  out os ["// unmarshall results"];
		  List.app (out os) (unmarshallResults results);
		  out os ["  return result;"])
	    end;
	    out os ["}\n"]
	  end

    fun generateGlue (file, iil) = let
	  val I.IIL{decls, clib_name=SOME lib, clib_version, clib_date, ...} = iil
	  val proto_file = replace_extension (file, "-lib", "hxx")
	  val clib_file = replace_extension (file, "-lib", "cxx")
	  val version = Option.getOpt(clib_version, "?")
	  val date = Option.getOpt(clib_date, Util.dateString())
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
		  out os ["\tML_CFunction() /* end marker */"];
		  out os ["    };"];
		  out os [];
		  out os ["static CLibInfo_t info = {"];
		  out os ["\t\"", lib, "\","];
		  out os ["\t\"", version, "\","];
		  out os ["\t\"", date, "\""];
		  out os ["    };"];
		  out os [];
		  out os ["ML_StaticCLibrary ", lib, "Lib("];
		  out os ["\t&info,"];
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
	  val (I.IIL{symtable, decls, ...}) = spec
	  val file_hxx = Util.replace_extension (srcFile, "", "hxx")
	  val proto_file = replace_extension (srcFile, "-lib", "hxx")
	  fun global os = let
		fun output_decl (I.Type(name, td)) = ()
		  | output_decl (I.Constant(name, c)) = ()
		  | output_decl (I.Operation(name, oper)) = output_operation os (name, oper)
		  | output_decl (I.CppQuote _) = ()
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
	    glob_symtable := SOME symtable;
	    withOutputFile (FILE_CXX, file_cxx, all);
	    generateGlue (srcFile, spec)
	  end 
	    handle ex => (print(concat["uncaught exception ", exnName ex, "\n"]); raise ex)

    val _ = CmdOptions.register ("new-rt", generate)

  end
