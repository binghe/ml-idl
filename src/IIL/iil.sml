(* iil.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * IDL internal language.
 *)

structure IIL = struct

  structure A = Atom
  structure S = AtomMap
    
  (* 
   * attributes
   *
   *)
    
  datatype direction = In
                     | Out
                     | InOut
    
  (* slight cheat: use the fact that integers are overloaded... *)
  datatype exp = E_Int of Int32.int
               | E_Word of Word32.word
               | E_Bool of bool
               | E_String of string
               | E_Char of char
               | E_Id of A.atom
    
  datatype field = Fld of {spec : type_spec, name : A.atom}
    
  (* TODO: need to inline nested structures? *)
  and type_spec = TS_Id of A.atom
                | TS_Void
                | TS_Real64
                | TS_Real32
(*                | TS_Int64 *)
                | TS_Int32
                | TS_Int16
                | TS_Int8
(*                | TS_Word64 *)
                | TS_Word32
                | TS_Word16
                | TS_Word8
                | TS_Bool
                | TS_String
                | TS_Char
                | TS_Ref of type_spec
                | TS_Ptr of type_spec
                | TS_Option of type_spec
                | TS_Struct of field list
                | TS_StructTag of A.atom
                | TS_Array of {spec: type_spec, size: exp}
                | TS_Dep of {id: A.atom, id_spec: type_spec, spec: type_spec}
                | TS_Case of {disc: exp,
                              disc_spec: type_spec,
                              cases: (exp * type_spec) list,
                              default: type_spec option}
                | TS_App of {oper: type_spec, app: exp}
    (* for SML only, for now --- correct when parameterizing *)
                | TS_Sml of string * string option (* sml_type, cpp_type *)
                | TS_Int
                | TS_Word
    
  and param = Prm of {
	  name : A.atom,	(* parameter name *)
	  spec : type_spec,	(* parameter type spec *)
	  dir : direction,	(* parameter direction *)
	  value : exp option	(* ??? [always NONE] *)
	}

  and const = Const of {value : exp, spec : type_spec}

  and oper = Oper of {context : bool,
                      exclude : bool,   (* if true, don't output protoype *)
                      pre : string option,
                      post : string option,
                      call : string option,
                      spec : type_spec,
                      params : param list}

  and typedef = TypeDef of {abstract : bool,
                            exclude : bool,   (* not really used... *)
                            spec : type_spec}

  datatype decl = Type of A.atom * typedef
                | Constant of A.atom * const
                | Operation of A.atom * oper
                | CppQuote of string

  datatype iil = IIL of {
	symtable: typedef S.map,
	struct_name : string option,
	sig_name : string option,
	clib_name : string option,
	clib_version : string option,
	clib_date : string option,
	decls : decl list
      }

  fun has_In In = true
    | has_In InOut = true 
    | has_In _ = false

  fun has_Out Out = true
    | has_Out InOut = true 
    | has_Out _ = false

  fun get_struct_name (NONE) = "Interface" 
    | get_struct_name (SOME (s)) = s

  fun get_sig_name (NONE) = "INTERFACE_SIG"
    | get_sig_name (SOME (s)) = s

  fun exp_to_string (E_Int i) = Int32.toString (i)
    | exp_to_string (E_Word w) = Word32.toString (w)
    | exp_to_string (E_Bool b) = Bool.toString (b)
    | exp_to_string (E_String s) = s
    | exp_to_string (E_Char c) = Char.toString (c)
    | exp_to_string (E_Id a) = A.toString (a)

  fun spec_to_string (TS_Id a) = A.toString a
    | spec_to_string (TS_Void) = "void"
    | spec_to_string (TS_Real32) = "real32"
    | spec_to_string (TS_Real64) = "real64"
(*    | spec_to_string (TS_Int64) = "int64" *)
    | spec_to_string (TS_Int32) = "int32"
    | spec_to_string (TS_Int16) = "int16"
    | spec_to_string (TS_Int8) = "int8"
(*    | spec_to_string (TS_Word64) = "word64" *)
    | spec_to_string (TS_Word32) = "word32"
    | spec_to_string (TS_Word16) = "word16"
    | spec_to_string (TS_Word8) = "word8"
    | spec_to_string (TS_Bool) = "bool"
    | spec_to_string (TS_String) = "string"
    | spec_to_string (TS_Char) = "char"
    | spec_to_string (TS_Ref (ts)) = concat ["ref ",spec_to_string (ts)]
    | spec_to_string (TS_Ptr (ts)) = concat ["ptr ",spec_to_string (ts)]
    | spec_to_string (TS_Option (ts)) = concat ["opt ",spec_to_string (ts)]
    | spec_to_string (TS_Struct fields) = concat [
	  "{",
	  String.concatWithMap ","
	    (fn (Fld{name, spec}) => concat [A.toString name, ":", spec_to_string spec])
	      fields,
	  "}"
	]
    | spec_to_string (TS_StructTag a) = concat ["struct ", A.toString a]
    | spec_to_string (TS_Array {spec,size}) =
	concat [spec_to_string (spec)," [",exp_to_string (size),"]"]
    | spec_to_string (TS_Dep {id,id_spec,spec}) = concat [
	  "dep ",A.toString (id),":",spec_to_string (id_spec),".",
	  spec_to_string (spec)
	]
    | spec_to_string (TS_Case _) = "<case>"
    | spec_to_string (TS_App {oper, app}) =
	concat ["(",spec_to_string (oper),") ", exp_to_string app]
    | spec_to_string (TS_Sml(str, NONE)) = concat ["sml <",str,">"]
    | spec_to_string (TS_Sml(str, SOME ty)) = concat ["sml <",str,", ",ty,">"]
    | spec_to_string (TS_Int) = "int"
    | spec_to_string (TS_Word) = "word"

  fun oper_to_string (spec, params) = let
	fun map_param (Prm{name, spec, dir, value}) = concat [
		case dir of In => "in " | Out => "out ",
		A.toString (name), ":", spec_to_string spec
	      ]
	in
	  concat ["(", Util.concatSep (",", map map_param params),") -> ", spec_to_string spec]
	end

  fun output (outS, IIL{decls, ...}) = let
	fun pr (name, str) = 
	      TextIO.output (outS, concat [" ", A.toString name, " : ",str, "\n"])
	fun decl (Type (name, TypeDef{spec, ...})) = pr (name, spec_to_string spec)
	  | decl (Constant (name, _)) = pr (name, "CONSTANT")
	  | decl (Operation (name, Oper {spec,params,...})) =
	      pr (name, oper_to_string (spec, params))
	  | decl _ = ()
	in
	  app decl decls
	end

end
