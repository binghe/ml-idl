(* concrete-runtime.sml
 *
 *)

structure ConcreteRuntime : CONCRETE_SIG = struct

  structure I = IIL
  structure A = Atom
  structure S = AtomMap
  structure F = Format
  open Util


  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())


  fun tuple (types) = let 
    val s = Util.concatSep (" * ",types)
  in
    case (types) 
      of [] => "unit"
       | [_] => s
       | _ => "("^s^")"
  end

  fun locate (s) = "cfun_"^(A.toString (s))

  fun typeFunction (spec,params) = let
    val in_params = List.filter (fn (I.Prm {dir,...}) =>
                                 I.has_In (dir)) params
    val out_params = List.filter (fn (I.Prm {dir,...}) =>
                                  I.has_Out (dir)) params
    fun mapParam (I.Prm {spec,...}) = typeSpec (spec)
    val in_types = map mapParam in_params
    val out_types = map mapParam out_params
    val result = let val s = typeSpec (spec)
    in
      if (s="unit") then [] else [s]
    end
    val in_type = tuple (in_types)
    val out_type = tuple (result@out_types)
  in
    concat [in_type," -> ",out_type]
  end

  and typeSpec (I.TS_Id (s)) = locate (s)
    | typeSpec (I.TS_Real32) = "Real.real"
    | typeSpec (I.TS_Real64) = "Real.real"
(*    | typeSpec (I.TS_Int64) = "Int32.int" *)
    | typeSpec (I.TS_Int32) = "Int32.int"
    | typeSpec (I.TS_Int16) = "Int32.int"
    | typeSpec (I.TS_Int8) = "Int32.int"
(*    | typeSpec (I.TS_Word64) = "Word32.word" *)
    | typeSpec (I.TS_Word32) = "Word32.word"
    | typeSpec (I.TS_Word16) = "Word32.word"
    | typeSpec (I.TS_Word8) = "Word32.word"
    | typeSpec (I.TS_Int) = "Int.int"
    | typeSpec (I.TS_Word) = "Word.word"
    | typeSpec (I.TS_Char) = "Char.char"
    | typeSpec (I.TS_Bool) = "Bool.bool"
    | typeSpec (I.TS_Void) = "unit"
    | typeSpec (I.TS_Ptr spec) = (* (typeSpec spec)^ *) "pointer"
    | typeSpec (I.TS_Ref spec) = (typeSpec spec)
    | typeSpec (I.TS_Option spec) = (typeSpec spec)^" option"
    | typeSpec (I.TS_String) = "String.string"
    | typeSpec (I.TS_Array {spec,...}) = (typeSpec spec)^" vector"
    | typeSpec (I.TS_Sml t) = t
    | typeSpec (I.TS_Struct fields) = tuple (map (fn (I.Fld {name,spec}) => 
                                                    typeSpec (spec)) fields)
    | typeSpec (I.TS_StructTag a) = "unit" (* not satisfactory, but hey... *)
    | typeSpec (I.TS_App {oper,...}) = typeSpec (oper)
    | typeSpec (I.TS_Dep {spec,...}) = typeSpec (spec)
    | typeSpec ts = fail ["ConcreteRuntime.typeSpec","unhandled type spec ",I.spec_to_string (ts)]

    

  fun findI p l = let
    fun findI' (i,p,[]) = NONE
      | findI' (i,p,x::xs) = if (p (x)) then SOME (x,i)
                             else findI' (i+1,p,xs)
  in
    findI' (0,p,l)
  end
    
  val table = ref (NONE) : (I.typedef S.map) option ref
  val clib_name = ref ("")

  val name = "Runtime"

  fun init (I.IIL {symtable,decls,clib_name=SOME (foo),...}) = 
      (clib_name := foo;
       table := SOME (symtable);
       "structure C = Unsafe.CInterface")
    | init _ = Error.error ["IDL file missing required information (e.g. clib_name)"]


  fun oper (name,spec,params) = 
    F.format "val cfun_%s : %s = C.c_function \"%s\" \"%s\"" 
       [F.STR (name), F.STR (typeFunction (spec,params)), F.STR (!clib_name), F.STR (name)]

  fun supportFiles (I.IIL {...}) = []

  fun lookup (a) =
    (case (!table)
       of NONE => fail ["ConcreteRuntime.lookup",
                              "table not initialized"]
        | SOME (st) => (case (S.find (st,a))
                          of NONE => fail ["ConcreteRuntime.lookup",
                                                 "cannot find definition of ",A.toString (a)]
                           | SOME (I.TypeDef {spec,...}) => spec))


  fun invalid_op_error (typ,opr) = fail  ["ConcreteRuntime.query",
                                                "Operation ",opr," not defined for type ",
                                                IIL.spec_to_string (typ)]
  fun call (n,result,params) = "cfun_"^n

  val id = "(fn (x) => x)"
  val error = "(fn _ => raise Fail \"Error!\")"
  val free = "(fn _ => ())"

  fun query (I.TS_Id (n),opr) = query (lookup (n),opr)

    | query (I.TS_Real32,"mk") = id
    | query (I.TS_Real32,"get") = id
    | query (I.TS_Real32,"free") = free

    | query (I.TS_Real64,"mk") = error
    | query (I.TS_Real64,"get") = error
    | query (I.TS_Real64,"free") = free

    | query (I.TS_Int,"mk") = id
    | query (I.TS_Int,"get") = id
    | query (I.TS_Int,"free") = free

    | query (I.TS_Word,"mk") = id
    | query (I.TS_Word,"get") = id
    | query (I.TS_Word,"free") = free
    
    | query (I.TS_Int32,"mk") = id
    | query (I.TS_Int32,"get") = id
    | query (I.TS_Int32,"free") = free

    | query (I.TS_Int16,"mk") = id
    | query (I.TS_Int16,"get") = id
    | query (I.TS_Int16,"free") = free

    | query (I.TS_Int8,"mk") = id
    | query (I.TS_Int8,"get") = id
    | query (I.TS_Int8,"free") = free

    | query (I.TS_Word32,"mk") = id
    | query (I.TS_Word32,"get") = id
    | query (I.TS_Word32,"free") = free

    | query (I.TS_Word16,"mk") = id
    | query (I.TS_Word16,"get") = id
    | query (I.TS_Word16,"free") = free

    | query (I.TS_Word8,"mk") = id  
    | query (I.TS_Word8,"get") = id 
    | query (I.TS_Word8,"free") = free
    
    | query (I.TS_Char,"mk") = id
    | query (I.TS_Char,"get") = id
    | query (I.TS_Char,"free") = free

    | query (I.TS_Bool,"mk") = id
    | query (I.TS_Bool,"get") = id
    | query (I.TS_Bool,"free") = free

    | query (I.TS_Void,"mk") = id
    | query (I.TS_Void,"get") = id
    | query (I.TS_Void,"free") = free
    
    | query (I.TS_Ref (t), "mk") = id
    | query (I.TS_Ref (t), "get") = id
    | query (I.TS_Ref (t), "free") = free
    
    | query (I.TS_Ptr (t), "mk") = id
    | query (I.TS_Ptr (t), "get") = id
    | query (I.TS_Ptr (t), "free") = free

    | query (I.TS_Option (t),"mk") = id
    | query (I.TS_Option (t),"get") = id
    | query (I.TS_Option (t),"free") = free

    | query (I.TS_String,"mk") = id
    | query (I.TS_String,"get") = id
    | query (I.TS_String,"free") = free

    | query (I.TS_Struct (fields),"mk") = 
        concat ["(fn {",
                concatSep (",",map (fn (I.Fld {name,spec,...}) => A.toString name) fields),
                "} => (",
                  concatSep (",",map (fn (I.Fld {name,spec,...}) => A.toString name) fields),
                  "))"]
    | query (typ as I.TS_Struct (fields),"free") = free
    | query (typ as I.TS_Struct (fields),opr) = 
        if (String.isPrefix "get_" opr)
          then case (findI (fn (I.Fld {name,...}) => ("get_"^(A.toString name) = opr)) fields)
                 of NONE => invalid_op_error (typ,opr)
                  | SOME (I.Fld {spec,name,...},i) => 
                      concat ["(fn (",
                              concatSep (",",mapI (fn (_,i) => "x_"^(Int.toString i)) fields),
                              ") => x_", Int.toString (i),")"]
        else  invalid_op_error (typ,opr)                    

    | query (I.TS_StructTag _,"mk") = id
    | query (I.TS_StructTag _,"get") = id
    | query (I.TS_StructTag _,"free") = free
       
    | query (I.TS_Array _, "mk") = "(fn s => fn x => x)"
    | query (I.TS_Array _, "free") = free
    | query (I.TS_Array {spec,...}, "get_sub") = "(fn s => fn v => fn i => Vector.sub (v,i))"

    | query (I.TS_Sml s, "mk") = "(fn x => x)"
    | query (I.TS_Sml s, "get") = "(fn x => x)"

    (* hack to get dep types to work during testing *)
    | query (_,"free") = free

    | query (typ,opr) = invalid_op_error (typ,opr)


end
