(* generate-sml-fn.sml
 *
 * The Natural mapping
 *
 *)

functor GenerateSMLFn (structure C: CONCRETE_SIG) = struct

  structure I = IIL
  structure A = Atom
  structure S = AtomMap
  structure F = Format
  structure G = GenerateSMLUtil
  open Util

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  fun name_constant (I.E_Int i) = Int32.toString i
    | name_constant (I.E_String s) = (Error.warning ["Using a string constant in case type"];s)
    | name_constant (I.E_Char c) = Char.toString c
    | name_constant (I.E_Bool b) = Bool.toString b
    | name_constant (I.E_Id a) = A.toString a
    
  fun output_case os (a,{disc,disc_spec,cases,default}) = let
    val defaultcst = case (default) 
                       of NONE => []
                        | SOME (s) => [(I.E_String "default",G.typeSpec (s))]
    val allcst = (map (fn (c,s) => (c,G.typeSpec (s))) cases)@defaultcst
    val dataty = concatSep (" | ",map (fn (c,c_spec) => F.format "%s_%s of %s"
                                                           [F.ATOM a, F.STR (name_constant (c)),
                                                            F.STR (c_spec)]) allcst)
  in
    outF os "datatype %s = %s" [F.ATOM a,F.STR (dataty)]
  end

  fun output_def os (a,spec) = 
    outF os "type %s = %s" [F.ATOM a,F.STR (G.typeSpec (spec))]

  fun output_type os (a,I.TS_Case c) = output_case os (a,c)
    | output_type os (a,I.TS_Dep {spec,...}) = output_type os (a,spec)
    | output_type os (a,I.TS_App _) = Error.error ["Type applications cannot be defined"]
    | output_type os (a,spec) = output_def os (a,spec)
    
    
  (****************************** SIGNATURE ******************************)
    
  structure NaturalSignature = struct

    fun output_oper os (a,I.Oper {spec,params,...}) = 
      outF os "val %s : %s" [F.ATOM a,F.STR (G.typeFunction (spec,params))]

    fun output_type_def os (a,I.TypeDef {spec,abstract,...}) = 
      if (abstract)
        then outF os "type %s" [F.ATOM a]
      else output_type os (a,spec)

    fun output_const os (a,I.Const {spec,...}) = 
      outF os "val %s : %s" [F.ATOM a, F.STR (G.typeSpec (spec))]

    fun generate (file,I.IIL {symtable,struct_name,sig_name,decls,...}) = let
      val n_str = I.get_struct_name (struct_name)
      val n_sig = I.get_sig_name (sig_name)
      fun output_decl os (I.Type (name,ts)) = output_type_def os (name,ts)
        | output_decl os (I.Constant (name,c)) = output_const os (name,c)
        | output_decl os (I.Operation (name,oper)) = output_oper os (name,oper)
        | output_decl os _ = ()
      fun f (os) = (outF os "signature %s = sig" [F.STR (n_sig)];
                    app (output_decl os) decls;
                    out os ["end\n"])
      val fsig = replace_extension (file,"-sig","sml")
    in
      Verbose.message2 ["Signature ",n_sig," in ",fsig];
      withOutputFile (FILE_SML,fsig,f);
      [fsig]
    end

  end


  (******************************* STRUCTURE ******************************)

  structure NaturalStructure = struct


    val glob_symtable = ref (NONE) : I.typedef S.map option ref

    fun findType (I.TS_Id (s)) = 
      (case (!glob_symtable)
         of NONE => fail ["GenerateSMLFn.findType",
                                "uninitialized global symbol table"]
          | SOME (st) => 
           (case (S.find (st,s))
              of SOME (I.TypeDef {spec,...}) => findType (spec)
               | NONE => Error.error [A.toString (s)," is not a type"]))
      | findType (t) = t
      
    fun queryMk (t) = C.query (t,"mk")
    fun queryGet (t) = C.query (t,"get")

    fun eval_int (I.E_Int i) = i
      | eval_int _ = Error.error ["Type mismatch: expecting integer"]

    fun eval_id (I.E_Id a) = a
      | eval_id _ = Error.error ["Type mismatch: expecting an identifier"]

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
      | str_constant _ = fail ["GenerateSMLFn.str_constant",
                                     "cannot convert constant to string"]

    fun marshall (I.TS_Id (s)) n p = marshall (findType (I.TS_Id (s))) (A.toString (s)) p 
      | marshall (I.TS_Ref spec) n p = concat ["(",
                                               queryMk (I.TS_Ref (spec)),
                                               " o ",
                                               marshall (spec) n p,
                                               ")"]
      | marshall (I.TS_Option spec) n p= concat ["(",
                                                 queryMk (I.TS_Option (spec)),
                                                 " o ",
                                                 " (fn NONE => NONE | SOME (v) => SOME (",
                                                 marshall (spec) n p," (v))))"]
      | marshall (ts as I.TS_Array {spec,size}) n p = concat ["(fn (v) => ",
                                                              queryMk (ts),
                                                              " ",
                                                              str_constant (p,size),
                                                              " (Vector.map (",
                                                              marshall (spec) n p,
                                                              ") v))"]
      | marshall (ts as I.TS_Struct (fields)) n p = let
          fun marshall' (I.Fld {name,spec=I.TS_App {oper,app}}) = ["  val m_",A.toString name," = ",
                                                                   marshall (I.TS_App {oper=oper,app=app}) "" p_empty," ",
                                                                   A.toString name," ", A.toString (eval_id (app))]
            | marshall' (I.Fld {name,spec}) = ["  val m_",A.toString name," = ",
                                               marshall (spec) "" p_empty, "(",A.toString name,")"]
        in
          concat ["(",queryMk (ts)," o (fn ({",
                  concatSep (",",map (fn (I.Fld {name,...}) => A.toString (name)) fields),
                  "}) => let ",
                  concat (map (concat o marshall') fields),
                  " in {",
                  concatSep (",",map (fn (I.Fld {name,...}) => concat [A.toString name, " = m_",A.toString name]) fields),
                  "} end))"]
        end
      | marshall (I.TS_Sml s) n p = queryMk (I.TS_Sml s)
      | marshall (ts as I.TS_Case {disc,disc_spec,cases,default}) n p = let
          fun map_case (exp,t) = concat ["(",n,"_",name_constant (exp)," v) => if (",
                                         str_constant (p,disc),"=",str_constant (p,exp),") then ",
                                         queryMk (ts), " ? (",
                                         marshall (t) n p, ")"]
          val default = case (default)
                          of NONE => []
                           | SOME (ts) => [(I.E_String "default",ts)]
        in
          concat ["(fn ",
                  concatSep (" | ", (map map_case (cases@default))),
                  ")"]
        end
      | marshall (I.TS_App {oper=I.TS_Dep {id,id_spec,spec},app}) n p = let
         val y = gensym ()
        in
          concat ["(fn (v) => fn (",y,") => ",marshall (spec) n (p_add (p,(id,I.E_String y)))," (v))"]
        end
      | marshall (ts) n p = queryMk (ts)

    fun unmarshall (I.TS_Id (s)) n p = unmarshall (findType (I.TS_Id (s))) (A.toString (s)) p
      | unmarshall (I.TS_Ref spec) n p = concat ["(", unmarshall (spec) n p, " o ",queryGet (I.TS_Ref (spec)),")"]
      | unmarshall (I.TS_Option spec) n p = concat ["((fn NONE => NONE | SOME (v) => SOME (",
                                                    unmarshall (spec) n p," (v))) o ",
                                                    queryGet (I.TS_Option (spec)),")"]
      | unmarshall (ts as I.TS_Array {spec,size}) n p = let
          val size = eval_int (size)   (* FIXME: eval_xxx should get 'p' to resolve identifiers *)
        in
          concat ["(fn (v) => Vector.tabulate (",
                  Int32.toString (size),
                  ", (fn (i) => ",
                  unmarshall (spec) n p," (",
                  C.query (ts,"get_i"),
                  " ",Int32.toString (size)," i v"]
        end
      | unmarshall (ts as I.TS_Struct (fields)) n p = let
          fun unmarshall' (I.Fld {name,spec=I.TS_App {oper,app}}) = ["  val m_",A.toString name," = ",
                                                                     unmarshall (I.TS_App {oper=oper,app=app}) "" p_empty,
                                                                     " (", C.query (ts,"get_"^(A.toString (name))),
                                                                     " (v)) ", A.toString (eval_id (app))]
            | unmarshall' (I.Fld {name,spec}) = ["  val m_",A.toString name," = ",
                                                 unmarshall (spec) "" p_empty, " (",C.query (ts,"get_"^(A.toString (name)))," (v))"]
        in
          concat ["(fn (v) => let ",
                  concat (map (concat o unmarshall') fields),
                  " in {",
                  concatSep (",",map (fn (I.Fld {name,...}) => concat [A.toString name," = m_",A.toString name]) fields),
                  "} end)"]
        end
      | unmarshall (I.TS_Sml s) n p = queryGet (I.TS_Sml s)
      | unmarshall (ts as I.TS_Case {disc,disc_spec,cases,default}) n p = "unmarshall<case>"
      | unmarshall (I.TS_App {oper=I.TS_Dep {id,id_spec,spec},app}) n p = let
          val y = gensym ()
        in
          concat ["(fn (v) => fn (",y,") => ",unmarshall (spec) n (p_add (p,(id,I.E_String y)))," (v)"]
        end
      | unmarshall (ts) n p = queryGet (ts)

    fun freeSpec (ts) = C.query (ts,"free")
      
    fun output_oper os (a,I.Oper {spec,params,...}) = let
      val n = A.toString a
      val _ = Verbose.message2 [" Processing operation: ",n]
      val s = G.typeFunction (spec,params)
      val in_params = List.filter (fn (I.Prm {dir,...}) =>
                                   I.has_In (dir)) params
      val out_params = List.filter (fn (I.Prm {dir,...}) =>
                                    I.has_Out (dir)) params
      fun mapParam (I.Prm {spec,...}) = G.typeSpec (spec)
      val in_types = map mapParam in_params
      val out_types = map mapParam out_params
      val in_names = map (fn (I.Prm {name,...}) => A.toString name) in_params
      val out_names = map (fn (I.Prm {name,...}) => A.toString name) out_params
      val result = let 
        val s = G.typeSpec (spec)
      in
        if (s="unit") then [] else [s]
      end
      val resultName = 
        case (result)
          of [] => []
           | [_] => ["result"]
           | _ => fail ["GenerateSMLFn.NaturalStructure.output_oper",
                              "more than one result"]
      fun marshall' (I.Prm {name,spec=I.TS_App {oper,app},...}) = 
            ["  val m_",A.toString name," = ",
             marshall (I.TS_App {oper=oper,app=app}) "" p_empty," ",A.toString name," ",
             A.toString (eval_id (app))]
        | marshall' (I.Prm {name,spec,...}) = ["  val m_",A.toString name," = ",
                                               marshall (spec) "" p_empty, "(",A.toString name,")"]
      fun unmarshall' (I.Prm {name,spec=I.TS_App {oper,app},...}) = 
            ["  val ",A.toString name," = ",
             unmarshall (I.TS_App {oper=oper,app=app}) "" p_empty," ",A.toString name," ",
             A.toString (eval_id (app))]
        | unmarshall' (I.Prm {name,spec,...}) = ["  val ",A.toString name," = (",
                                                 unmarshall (spec) "" p_empty, " m_", A.toString name,")"]
    in
      out os [C.oper (n,spec,params)];
      out os ["fun ",n,"(",concatSep (",",in_names),") = let"];
      app (out os) (map marshall' in_params);
      out os ["  val (",concatSep (",",map (fn (s) => ("m_"^s)) (resultName@out_names)),
              ") = ", C.call (n,spec,params)," (",
              concatSep (",",map (fn (s) => ("m_"^s)) in_names),")"];
      app (out os) (map unmarshall' out_params);
      (case (result)
         of [_] => out os ["  val result = ",unmarshall (spec) "" p_empty, "(m_result)"]
          | _ => ());
      app (out os) (map (fn (I.Prm {name,spec,...}) => let
        val s = freeSpec (spec)
      in
                         ["  val _ = ",s," (m_",A.toString name,")"]
      end) params);
      out os ["in"];
      out os ["  (",concatSep (",",resultName@out_names),")"];
      out os ["end"]
    end


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
    | typeSpec (I.TS_Sml (t, _)) = t
    | typeSpec (I.TS_Struct fields) = tuple (map (fn (I.Fld {name,spec}) => 
                                                    typeSpec (spec)) fields)
    | typeSpec (I.TS_StructTag a) = "unit" (* not satisfactory, but hey... *)
    | typeSpec (I.TS_App {oper,...}) = typeSpec (oper)
    | typeSpec (I.TS_Dep {spec,...}) = typeSpec (spec)
    | typeSpec ts = fail ["GenerateSMLFn.typeSpec","unhandled type spec ",I.spec_to_string (ts)]

  fun output_cfun_def os (a,spec) = 
    outF os "type cfun_%s = %s" [F.ATOM a,F.STR (typeSpec (spec))]

  fun output_cfun_type os (a,I.TS_Case c) = fail ["GenerateSMLFn.output_cfun_type","Case type"]
    | output_cfun_type os (a,I.TS_Dep {spec,...}) = fail ["GenerateSMLFn.output_cfun_type","Dep type"]
    | output_cfun_type os (a,I.TS_App _) = Error.error ["Type applications cannot be defined"]
    | output_cfun_type os (a,spec) = output_cfun_def os (a,spec)
  
    fun output_type_def os (a,I.TypeDef {spec,...}) = 
      (output_type os (a,spec);
       (* well, this kills the genericity of this file, doesn't it?
        * This will be changed in v1.2, surely
        *)
       output_cfun_type os (a,spec))
      
    fun output_const os (a,I.Const {value,spec,...}) = let
      val v = case (value)
                of I.E_Int (i) => concat [Int32.toString (i),
                                          " : ",
                                          G.typeSpec (spec)]
                | I.E_Word (w) => concat ["0wx",
                                          Word32.toString w,
                                          " : ",
                                          G.typeSpec (spec)]
                 | I.E_Bool (b) => Bool.toString b
                 | I.E_String (s) => s
                 | I.E_Char (c) => "#\""^(Char.toString c)^"\""
                 | I.E_Id _ => fail ["GenerateSMLFn.output_const",
                                           "cannot handle ids in constants just yet"]
    in
      out os ["val ",A.toString a," = ", v]
    end
  
  
    fun generate (file, ir as I.IIL {symtable,struct_name,sig_name,decls,...}) = let
      val n_str = I.get_struct_name (struct_name)
      val n_sig = I.get_sig_name (sig_name)
      fun output_decl os (I.Type (a,td)) = output_type_def os (a,td)
        | output_decl os (I.Constant (a,c)) = output_const os (a,c)
        | output_decl os (I.Operation (a,oper)) = output_oper os (a,oper)
        | output_decl os _ = ()
      fun f (os) = (out os ["structure ", n_str, " : ", n_sig, " = struct"];
                    out os [C.init (ir)];
                    app (output_decl os) decls;
                    out os ["end\n"])
      val fsml = replace_extension (file,"","sml")
    in
      Verbose.message2 ["Structure ",n_str," in ",fsml];
      glob_symtable := SOME (symtable);
      withOutputFile (FILE_SML,fsml,f);
      [fsml]
    end 
  
  end


  fun generate (file, ir) = let
    val _ = Verbose.message1 ["Generating SML code (",C.name," interface)"]
    val sig_files = NaturalSignature.generate (file,ir)
    val str_files = NaturalStructure.generate (file,ir)
  in 
    (C.supportFiles (ir))@sig_files@str_files 
  end 


end

