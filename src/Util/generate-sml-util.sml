
structure GenerateSMLUtil = struct

  structure I = IIL
  structure A = Atom

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  (* snatched from generate-sml-fn.sml
   * this should be put somewhere intelligent
   *)

  fun tuple (types) = let 
    val s = Util.concatSep (" * ",types)
  in
    case (types) 
      of [] => "unit"
       | [_] => s
       | _ => "("^s^")"
  end

  fun locate (s) = A.toString (s)

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
    | typeSpec (I.TS_Sml (t, _)) = t
    | typeSpec (I.TS_Struct fields) = 
        concat ["{",
                Util.concatSep (",",map (fn (I.Fld {name,spec}) => 
                                         concat [A.toString (name),":",
                                                 typeSpec (spec)]) fields),
                "}"]
    | typeSpec (I.TS_StructTag a) = "unit" (* not satisfactory, but hey... *)
    | typeSpec (I.TS_App {oper,...}) = typeSpec (oper)
    | typeSpec (I.TS_Dep {spec,...}) = typeSpec (spec)
    | typeSpec ts = fail ["GenerateSMLUtil.typeSpec","unhandled type spec ",I.spec_to_string (ts)]

end
