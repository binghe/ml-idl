(* idl-translate.sml
 *
 * Translate an IDL parse tree into IIL
 *)

structure IdlTranslate = struct

  structure I = IIL
  structure D = IdlParseTree
  structure A = Atom
  structure S = AtomMap

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  val mapP = List.mapPartial

  (* this is used to determine during type conversion that a type 
   * definition is in fact abstract, e.g.
   *   typedef struct t t_type;
   *)
  val abstract_flag = ref (false)

  val new_tag = let 
    val counter = ref (0)
  in
    fn () => let
      val v = !counter
      val r = "__tag__"^(Int.toString (v))^"__"
    in
      counter := v+1;
      A.atom r
    end
  end

  (* this ref cell holds an association of all the structure tags encountered, 
   * and their corresponding definitions. I'm not sure it will ever be needed, but
   * it's here for future extension. This info is not propagated to the IIL
   *)
  val taggedTable : (A.atom * I.type_spec) list ref = ref []
    
  fun toStringOpt NONE = NONE
    | toStringOpt (SOME a) = SOME (A.toString a)

  datatype attribute = A_In
                     | A_Out
                     | A_SwitchType of D.switch_type_spec
                     | A_SwitchIs of D.attr_var
                     | A_String
                     | A_SmlType of string
                     | A_SizeIs of (D.attr_var) list
                     | A_Ignore
                     | A_Ref
                     | A_Unique
                     | A_Ptr
                     | A_SmlContext
                     | A_Exclude
                     | A_Abstract
                     | A_Pre of string
                     | A_Post of string
                     | A_Call of string
    
  val default_ptr_attr = ref (A_Ref)

  (* remove surrounding quotes *)
(*  fun stripString (s) = let
    val s' = Substring.all (s)
    val l = Substring.size (s')
  in
    Substring.string (Substring.triml 1 (Substring.trimr 1 s'))
  end
*)
    
  (* convert attributes
   *)

  fun flatten_param_attr (D.A_ParamField fa) = flatten_field_attr (fa)
    | flatten_param_attr (D.A_In) = SOME (A_In)
    | flatten_param_attr (D.A_Out) = SOME (A_Out)
  and flatten_type_attr (D.A_TypeUsage ua) = flatten_usage_attr (ua)
    | flatten_type_attr (D.A_TypeUnion ua) = flatten_switch_type_attr (ua)
    | flatten_type_attr (D.A_TypePtr pa) = flatten_ptr_attr (pa)
    | flatten_type_attr (D.A_Exclude) = SOME (A_Exclude)
    | flatten_type_attr (D.A_Abstract) = SOME (A_Abstract)
    | flatten_type_attr _ = NONE
  and flatten_usage_attr (D.A_String) = SOME (A_String)
    | flatten_usage_attr (D.A_SmlType s) = SOME (A_SmlType s)
    | flatten_usage_attr _ = NONE
  and flatten_field_attr (D.A_SizeIs l) = SOME (A_SizeIs l)
    | flatten_field_attr (D.A_FieldUsage ua) = flatten_usage_attr (ua)
    | flatten_field_attr (D.A_FieldUnion ua) = flatten_switch_is_attr (ua)
    | flatten_field_attr (D.A_Ignore) = SOME (A_Ignore)
    | flatten_field_attr (D.A_FieldPtr pa) = flatten_ptr_attr (pa)
    | flatten_field_attr _ = NONE
  and flatten_ptr_attr (D.A_Ref) = SOME (A_Ref)
    | flatten_ptr_attr (D.A_Unique) = SOME (A_Unique)
    | flatten_ptr_attr (D.A_Ptr) = SOME (A_Ptr)
  and flatten_operation_attr (D.A_OperationUsage ua) = flatten_usage_attr (ua)
    | flatten_operation_attr (D.A_OperationPtr pa) = flatten_ptr_attr (pa)
    | flatten_operation_attr (D.A_SmlContext) = SOME (A_SmlContext)
    | flatten_operation_attr (D.A_Pre s) = SOME (A_Pre s)
    | flatten_operation_attr (D.A_Post s) = SOME (A_Post s)
    | flatten_operation_attr (D.A_Call s) = SOME (A_Call s)
    | flatten_operation_attr (D.A_ExcludeOper) = SOME (A_Exclude)  (* ditch artificial distinction *)
    | flatten_operation_attr _ = NONE
  and flatten_switch_is_attr (D.A_SwitchIs l) = SOME (A_SwitchIs l)
  and flatten_switch_type_attr (D.A_SwitchType ts) = SOME (A_SwitchType ts)

  fun fromW (w) = if (w > 0wx7fffffff)
                    then let 
                      val w' = (0wxffffffff) - w
                    in 
                      Int32.fromLarge (~ (Word32.toLargeInt (w')) -1) 
                    end
                  else Int32.fromLarge (Word32.toLargeInt (w))

  fun eval_int_exp (D.CE_Int i) = i
    | eval_int_exp (D.CE_Word w) = fromW (w)

(* HEREHEREHERE *)
(*  convert word32 to words is a bit of work... *)

  (* FIXME: add some error checking here... *)
  fun convert_constant (D.CT_Integer (D.SmlInt),D.CE_Integer (D.CE_Int i)) = I.E_Int (i)
    | convert_constant (D.CT_Integer (D.SmlWord), D.CE_Integer (D.CE_Word w)) = I.E_Word (w)
    | convert_constant (D.CT_Integer (D.SmlWord), D.CE_Integer (D.CE_Int i)) = I.E_Word (Word32.fromLargeInt (Int32.toLarge i))
    | convert_constant (D.CT_Integer (D.Signed (D.Int)),D.CE_Integer (D.CE_Int i)) = I.E_Int (i)
    | convert_constant (D.CT_Integer (D.Unsigned (D.UInt)),D.CE_Integer (D.CE_Word w)) = I.E_Word (w)
    | convert_constant (D.CT_Integer (D.Unsigned (D.UInt)),D.CE_Integer (D.CE_Int i)) = I.E_Word (Word32.fromLargeInt (Int32.toLarge i))
    | convert_constant (_,D.CE_String (s)) = I.E_String (s)
    | convert_constant (_,D.CE_True) = I.E_Bool (true)
    | convert_constant (_,D.CE_False) = I.E_Bool (false)
    | convert_constant (_,D.CE_Null) = I.E_Int (0)    (* FIXME *)
    | convert_constant (_,D.CE_Id (a)) = I.E_Int (0)  (* FIXME *)
    | convert_constant (_,_) = fail ["IdlTranslate.convert_constant","Mismatched value and type"]
    
  fun convert_enum (sl) = let
    fun f (D.Id (s),(n,l)) = (n+1,(s,n)::l)
      | f (D.IdValue (s,exp), (n,l)) = (n,(s,eval_int_exp (exp))::l)
    val (_,l) = foldr f (0:Int32.int,[]) sl
  in 
    l (* map (fn (s,v) => (s,v)) l *)
  end

  fun get_ptr_attr (A_Ptr) = SOME (A_Ptr)
    | get_ptr_attr (A_Ref) = SOME (A_Ref)
    | get_ptr_attr (A_Unique) = SOME (A_Unique)
    | get_ptr_attr _ = NONE

  fun get_string_attr (A_String) = SOME (A_String)
    | get_string_attr _ = NONE

  fun get_size_attr (A_SizeIs v) = SOME (A_SizeIs v)
    | get_size_attr _ = NONE

  fun get_dir_attr (A_In) = SOME (A_In)
    | get_dir_attr (A_Out) = SOME (A_Out)
    | get_dir_attr _ = NONE

  fun get_type_attr (A_SwitchType (ty)) = SOME (A_SwitchType (ty))
    | get_type_attr _ = NONE 

  fun get_union_attr (A_SwitchIs v) = SOME (A_SwitchIs v)
    | get_union_attr _ = NONE 
    
  fun get_array_attr (A_SizeIs v) = SOME (A_SizeIs v)
    | get_array_attr _ = NONE

  fun extract_array_attrs l = mapP get_array_attr l

  fun extract_switch_is l = 
    case (mapP get_union_attr l)
      of [] => NONE
       | [A_SwitchIs (D.Var v)] => SOME (v)
       | _ => Error.error ["Multiple union attributes or multiple/unrecognized variables"]

  fun extract_single_ptr_attr l = 
    case (mapP get_ptr_attr l)
      of [] => !default_ptr_attr
       | [v] => v
       | _ => Error.error ["Multiple pointer attributes"]

  fun extract_sml_type [] = NONE
    | extract_sml_type ((A_SmlType (t))::xs) = SOME (t)
    | extract_sml_type (_::xs) = extract_sml_type (xs)

  fun extract_type_attrs l = mapP get_type_attr l    

  fun extract_size_is l = 
    case (mapP get_array_attr l)
      of [] => NONE
       | [A_SizeIs ([D.Var v])] => SOME (v)
       | _ => Error.error ["Unrecognized array attribute or multiple/unrecognized variables"]

  fun get_abstract_attr (A_Abstract) = SOME (A_Abstract)
    | get_abstract_attr _ = NONE

  fun has_abstract_attr l = 
    case (mapP get_abstract_attr l)
      of [] => false
       | [_] => true
       | _ => fail ["IdlTranslate.has_abstract_attr","Multiple [abstract] attributes"]

  fun get_exclude_attr (A_Exclude) = SOME (A_Exclude)
    | get_exclude_attr _ = NONE

  fun has_exclude_attr l = 
    case (mapP get_exclude_attr l)
      of [] => false
       | [_] => true
       | _ => fail ["IdlTranslate.has_exclude_attr","Multiple [exclude] abttributes"]

  fun has_string_attr l = 
    case (mapP get_string_attr l)
      of [] => false
       | [_] => true
       | _ => fail ["IdlTranslate.has_string_attr","Multiple [string] attributes"]

  and has_size_attr l = 
    case (mapP get_size_attr l)
      of [] => false
       | [_] => true
       | _ => fail ["IdlTranslate.has_size_attr","Multiple [size_is] attributes"]

  and has_context_attr [] = false
    | has_context_attr (A_SmlContext::xs) = true
    | has_context_attr (_::xs) = has_context_attr (xs)

  and has_call_attr [] = NONE
    | has_call_attr ((A_Call s)::xs) = SOME (s)
    | has_call_attr (_::xs) = has_call_attr (xs)

  and has_pre_attr [] = NONE
    | has_pre_attr ((A_Pre s)::xs) = SOME (s)
    | has_pre_attr (_::xs) = has_pre_attr (xs)

  and has_post_attr [] = NONE
    | has_post_attr ((A_Post s)::xs) = SOME (s)
    | has_post_attr (_::xs) = has_post_attr (xs)

  and convert_dir_attr l = let
    val l' = mapP get_dir_attr l
    val in_attr = List.exists (fn A_In => true | _ => false) l'
    val out_attr = List.exists (fn A_Out => true | _ => false) l'
  in
    if (in_attr andalso out_attr) 
      then (*I.InOut*) 
        fail ["IdlTranslate.convertDir",
                    "[in,out] attributes disabled until further notice"]
    (* I.InOut *)
    else if in_attr
           then I.In
    else if out_attr
           then I.Out
         else Error.error ["No directional attribute in parameter declaration"]
  end


  datatype declarator = Id of A.atom
                      | Array of D.array_declarator
                      | Function of D.function_declarator
                      | Pointer of declarator
  fun wrap_direct (D.D_Id (a)) = Id (a)
    | wrap_direct (D.D_PId (a)) = Id (a)
    | wrap_direct (D.D_Array (a)) = Array (a)
    | wrap_direct (D.D_Function (a)) = Function (a)
  fun wrap (D.Decl ([],dd)) = wrap_direct (dd)
    | wrap (D.Decl (x::xs,dd)) = Pointer (wrap (D.Decl (xs,dd)))
    
  (* switch_type_spec -> type_spec *)
  fun lift (D.SW_Integer i) = D.TS_Simple (D.Integer (D.PrimitiveInteger i))
    | lift (D.SW_Char c) = D.TS_Simple (D.Character c)
    | lift (D.SW_Bool) = D.TS_Simple (D.Bool)
    | lift (D.SW_Id a) = D.TS_Simple (D.TS_Id (a))
    
  fun convert_prim_integer (D.SmlInt) = I.TS_Int
    | convert_prim_integer (D.SmlWord) = I.TS_Word
    | convert_prim_integer (D.Signed (D.Long)) = I.TS_Int32
    | convert_prim_integer (D.Signed (D.Short)) = I.TS_Int16
    | convert_prim_integer (D.Signed (D.Small)) = I.TS_Int8
    | convert_prim_integer (D.Signed (D.Int)) = I.TS_Int32
    | convert_prim_integer (D.Unsigned (D.ULong)) =  I.TS_Word32
    | convert_prim_integer (D.Unsigned (D.UShort)) =  I.TS_Word16
    | convert_prim_integer (D.Unsigned (D.USmall)) =  I.TS_Word8
    | convert_prim_integer (D.Unsigned (D.UInt)) =  I.TS_Word32
    | convert_prim_integer (D.Unsigned (D.LongU)) =  I.TS_Word32
    | convert_prim_integer (D.Unsigned (D.ShortU)) =  I.TS_Word16
    | convert_prim_integer (D.Unsigned (D.SmallU)) =  I.TS_Word8

  fun convert_const_type_spec (D.CT_Integer i) = convert_prim_integer (i)
    | convert_const_type_spec (D.CT_Char) =  (I.TS_Char)
    | convert_const_type_spec (D.CT_Bool) =  (I.TS_Bool)
    | convert_const_type_spec (D.CT_VoidPtr _) = fail ["convert_const_type_spec","void ptr"]
    | convert_const_type_spec (D.CT_CharPtr _) = I.TS_String

  (*
   * convert a type_spec to an attr_type_spec depending on the attributes 
   *
   *)
  fun mk_array_info (attrs) = 
    case (extract_size_is (attrs))
      of SOME (v) => {sizeis=v}
       | NONE => fail ["No array info provided"]
  fun mk_attr_spec (attrs,spec) = spec(*
    case (extract_switch_is attrs) (*(mapP attrFieldUnion attrs)) *)
      of NONE => (case (extract_array_attrs (attrs)) 
                    of [] => spec (* I.ATS_Type (spec) *)
                     | l => fail ["IdlTranslate.mk_attr_spec","Arrays not yet supported"]) (* I.ATS_Array (mk_array_info (l),spec)) *)
       | SOME (v) => (* I.ATS_Union (v,spec) *) fail ["IdlTranslate.mk_attr_spec","Unions not yet supported"]
*)
  fun convert_params (D.Empty) = []
    | convert_params (D.EmptyVoid) = []
    | convert_params (D.Params p) = p

  and extract_array_info (D.ArrayOne (d,abopt)) = (wrap_direct (d),abopt)
    | extract_array_info (_) = fail ["IdlTranslate.extract_array_info","Not supported"]

  and convert_declarator (ptr_attr,has_string,has_size,t,Id (s)) = (t,s)
    | convert_declarator (p_attr,s_attr,_,t,Pointer (Function (D.Function (decl,params)))) = let
        val (spec,decl) = convert_declarator (!default_ptr_attr,false,NONE,t,wrap_direct decl)
      in
        (*
        case (p_attr)
          of A_Ptr => (I.TS_Function {result=spec,
                                      params=map convert_param_declarator (convert_params params)},decl)
           | A_Ref => (I.TS_Function {result=spec,
                                      params=map convert_param_declarator (convert_params params)},decl)
           | A_Unique => (I.TS_Option (I.TS_Function {result=spec,
                                                      params=map convert_param_declarator (convert_params params)}),decl)
           | _ => fail ["IdlTranslate.explode_declarator/mapDecl","unmatched case"]
            *)
        fail ["IdlTranslate.convert_declarator",
                    "cannot handle functions just yet"]
      end
    | convert_declarator (p_attr,s_attr,NONE,t,Pointer (d)) = let 
        val (spec,decl) = convert_declarator (!default_ptr_attr,false,NONE,t,d) 
        fun mk_string ( (I.TS_Char),decl,b) = 
              if b 
                then (I.TS_Option (I.TS_String),decl)
              else (I.TS_String,decl)
          | mk_string _ = Error.error ["Unappropriate string attribute"]
      in
        case (p_attr,s_attr)
          of (A_Ptr,false) => (I.TS_Ptr (spec),decl)
           | (A_Ref,false) => (I.TS_Ref (spec),decl)
           | (A_Unique,false) => (I.TS_Option (I.TS_Ref (spec)),decl)
           | (A_Ptr,true) => mk_string (spec,decl,false)
           | (A_Ref,true) => mk_string (spec,decl,false)
           | (A_Unique,true) => mk_string (spec,decl,true)
           | _ => fail ["IdlTranslate.explode_declarator/mapDecl","unmatched case"]
      end
    | convert_declarator (_,_,SOME (v),t,Pointer (d)) = let
        val (spec,decl) = convert_declarator (!default_ptr_attr,false,NONE,t,d)
        val var = A.atom (Util.gensym ())
      in
        (I.TS_App {oper = I.TS_Dep {id=var,id_spec=I.TS_Int32,
                                    spec=I.TS_Array {spec=spec,size=I.E_Id (var)}},
                   app = I.E_Id (v)},decl)
      end
    | convert_declarator (_,_,NONE,t,Array d) = let
        val (d,abopt) = extract_array_info (d)
        val (spec,decl) = convert_declarator (!default_ptr_attr,false,NONE,t,d)
      in
        (I.TS_Array {spec=spec,size=case (abopt)
                                      of NONE => Error.error ["No array size specified"]
                                      | SOME (D.BoundAst _ ) => fail ["IdlTranslate.convert_declarator","Huh? BoundAst?"]
                                      | SOME (D.BoundInt (ice)) => I.E_Int (eval_int_exp (ice))
                                      | SOME (D.BoundId (a)) => I.E_Id (a)},decl)
      end
    | convert_declarator (_,_,SOME (v),t,Array d) = let
        val (d,abopt) = extract_array_info (d)
        val _ = case (abopt) of NONE => () | _ => Error.warning ["Ignoring explicit array bound"]
        val (spec,decl) = convert_declarator (!default_ptr_attr,false,NONE,t,d)
        val var = A.atom (Util.gensym ())
      in
        (I.TS_App {oper=I.TS_Dep {id=var,id_spec=I.TS_Int32,
                                  spec=I.TS_Array {spec=spec,size=I.E_Id (var)}},
                   app=I.E_Id (v)},decl)
      end
    | convert_declarator (p_attr,s_attr,_,t,Function (D.Function (decl,params))) = 
        Error.error ["cannot have a non-pointer function"]

  and explode_declarator (ptr_attr,has_string,has_size,t,d) = let 
    val d = map wrap d
  in
    map (fn d => convert_declarator (ptr_attr,has_string,has_size,t,d)) d
  end

  and convert_field (D.Field (attropt,spec,decls)) = let
    val attrs = case (attropt) of NONE => [] | SOME (ats) => ats
    val attrs = mapP flatten_field_attr attrs
    val ptr = extract_single_ptr_attr (attrs)
    val typ = [] 
    val sml_type = extract_sml_type (attrs)
    val spec' = convert_type_spec (sml_type,typ,spec)
    val list = explode_declarator (ptr,
                                   has_string_attr (attrs),
                                   extract_size_is (attrs) ,spec',decls)
  in
    map (fn (spec,name) => I.Fld {spec= mk_attr_spec (attrs,spec),
                                   name=name}) list
  end

  and convert_sml_value (NONE) = Error.error ["Need sml_type attribute for every sml_value type"]
    | convert_sml_value (SOME t) = I.TS_Sml (Util.stripString (t))

  and convert_type_spec (sml_type,_,D.TS_Simple (D.SmlValue)) = convert_sml_value (sml_type)
    | convert_type_spec (_,_,D.TS_Simple (spec)) = convert_simple_type_spec (spec)
    | convert_type_spec (_,ts,D.TS_Constructed (spec)) = convert_constructed_type_spec (ts,spec)

  and convert_integer_type (D.PrimitiveInteger (i)) = convert_prim_integer (i)
(*    | convert_integer_type (D.HInt) =  I.TS_Int64
    | convert_integer_type (D.HUInt) = I.TS_Word64
    | convert_integer_type (D.UHInt) = I.TS_Word64 *)
    | convert_integer_type _ = Error.error ["Unsupported large integer type"]

  and convert_character_type (D.UChar) = I.TS_Char
    | convert_character_type (D.Char) =  I.TS_Char
    | convert_character_type (D.UWChar) = I.TS_Char
    | convert_character_type (D.WChar) =  I.TS_Char

  and convert_simple_type_spec (D.Float) =  I.TS_Real32
    | convert_simple_type_spec (D.Double) =  I.TS_Real64
    | convert_simple_type_spec (D.Integer (i)) = convert_integer_type (i)
    | convert_simple_type_spec (D.Character (c)) = convert_character_type (c)
    | convert_simple_type_spec (D.Bool) =  (I.TS_Bool)
    | convert_simple_type_spec (D.Byte) =  I.TS_Word8
    | convert_simple_type_spec (D.Void) =  I.TS_Void
    | convert_simple_type_spec (D.String) =  I.TS_String
    | convert_simple_type_spec (D.Handle) = fail ["convert_simple_type_spec","handle_t"]
    | convert_simple_type_spec (D.ErrorStatus) = fail ["convert_simple_type_spec","predefined type"]
    | convert_simple_type_spec (D.IsoLatin1) = fail ["convert_simple_type_spec","predefined type"]
    | convert_simple_type_spec (D.IsoMulti) = fail ["convert_simple_type_spec","predefined type"]
    | convert_simple_type_spec (D.IsoUcs) = fail ["convert_simple_type_spec","predefined type"]
    | convert_simple_type_spec (D.TS_Id (a)) = I.TS_Id (a)
    | convert_simple_type_spec (_) = fail ["convert_simple_type_spec","unrecognized type"]

  and convert_constructed_type_spec (_,D.Struct (fields)) = convert_struct_type (new_tag (),fields)
    | convert_constructed_type_spec (ts,D.Union (ut)) = convert_union_type (ts,ut)
    | convert_constructed_type_spec (_,D.Enum (ids)) = fail ["IdlTranslate.convert_constructed_type_spec",
                                                                   "Enums not yet supported"]
    | convert_constructed_type_spec (ts,D.TaggedD (tg)) = convert_tagged_decl (ts,tg)

  and convert_tagged_decl (_,D.TG_Struct (D.S_Tag a)) =  I.TS_StructTag (a)
        (* ( abstract_flag := true;
             convert_struct_type (a,[])) *)    
    | convert_tagged_decl (_,D.TG_Struct (D.S_TagBody (a,ml))) = let
        val s = convert_struct_type (a,ml)
      in
        taggedTable := (a,s)::(!taggedTable);
        s
      end
    | convert_tagged_decl (_,D.TG_Union (D.U_Tag _)) = fail ["convert_tagged_decl",
                                                                   "tagged union with no body"]
    | convert_tagged_decl (ts,D.TG_Union (D.U_TagBody (tu))) = convert_tagged_union (ts,tu)

  and convert_struct_type (tag,fields) = I.TS_Struct (List.concat (map convert_field fields))

  and convert_union_type (ts,D.EUnion (switch, a, name, body)) = convert_union (ts,new_tag(),switch,a,name,body)
    | convert_union_type (_,D.NEUnion _) = fail ["convert_union_type","non-encapsulated union"]
    
  and convert_tagged_union (ts,D.TaggedEUnion (tag,switch,a,name,body)) = convert_union (ts,tag,switch,
                                                                                         a,name,body)
    | convert_tagged_union (_,D.TaggedNEUnion _) = fail ["convert_tagged_union","non-encapsulated union"]

  and convert_union (ts,tag,switch,a,name,D.Body (cases)) = let
    fun find_default [] = Error.error ["union with no default case defined"]
      | find_default ((D.DefaultCase (arm))::xs) = arm
      | find_default (_::xs) = find_default (xs)
    fun fold_cases (D.DefaultCase _,xs) = xs
      | fold_cases (D.UnionCase (arg),xs) = arg::xs
    val default = find_default cases
    val cases = foldr fold_cases [] cases
    (* get switchtype attribute if it exists *)
    (* SwitchType is the only type attribute *)
    val switchtype = foldr (fn (A_SwitchType (ty),_) => SOME(convert_type_spec (NONE,[],lift (ty)))
                             | (_,_) => fail ["IdlTranslate.convert_union","unmatched case"]) NONE ts 
    fun convert_case (v,NONE) = (convert_constant v,[])
      | convert_case (v,SOME f) = (convert_constant v, (convert_field (f)))
    fun get_switch_type (NONE,NONE) = Error.error ["No switch specified for union type"]
      | get_switch_type (SOME (ts),NONE) = ts
      | get_switch_type (NONE, SOME ts) = convert_type_spec (NONE,[],lift (ts))
      | get_switch_type (SOME _, SOME _) = Error.error ["Multiple switches in union"]
    fun get_switch_name (NONE) = NONE
      | get_switch_name (SOME (_,st)) = SOME (st)
    fun convert_default (D.UnionArm NONE) = NONE
      | convert_default (D.UnionArm (SOME f)) = SOME (convert_field f)
  in
    fail ["IdlTranslate.convert_union","Unions not yet supported"]
    (*I.TS_Union {tag=tag,
                 switch_type = get_switch_type (switchtype,SOME switch),
                 switch_name = SOME (a), (* get_switch_name (SOME switch), *)
                 cases = List.concat (map (fn (vl,D.UnionArm f) => map (fn (v) => convert_case (v,f)) 
                                           vl)
                                      cases),
                 default= convert_default (default)} *)
  end    (* painful -- think about this later *)
    
  and convert_param_declarator (D.Param (attrs,spec,decl)) = let
    val attrs = mapP flatten_param_attr attrs
(*    val flds = mapP attrParamField attrs *)
    val ptr = extract_single_ptr_attr (attrs)
    val dir = convert_dir_attr (attrs)
    val typ = [] (* extract_type_attrs (mapP attrFieldUnion flds) *)
    val sml_type = extract_sml_type (attrs)
    val spec' = convert_type_spec (sml_type,typ,spec)
    val list = explode_declarator (ptr,
                                   has_string_attr (attrs),
                                   extract_size_is (attrs),spec',[decl])
  in
    case list 
      of [(spec,name)] => let 
        val spec' = if (I.has_Out dir)
                      then case (spec) 
                             (* convert full pointers to ref pointers, the default *)
                             (* fishy *)
                             of I.TS_Ptr (s) => I.TS_Ref (s)
                              | _ => spec
                    else spec
      in
        I.Prm {dir = dir,
                spec=mk_attr_spec (attrs,spec'),
                name=name,
                value=NONE}
      end
       | _ => fail ["convert_param_declarator","multiple param declarators"]
  end


  fun convert_type_decl {attrs, spec, decls} = let
    val attrs = mapP flatten_type_attr attrs
    val ptr = extract_single_ptr_attr (attrs)
    (* next, convert the specs *)
    val typ = extract_type_attrs (attrs)
    val sml_type = extract_sml_type (attrs)
    val spec' = convert_type_spec (sml_type,typ,spec)
    (* explode the declarators list *)
    val list = explode_declarator (ptr,
                                   has_string_attr (attrs),
                                   NONE,spec',decls)
  in
    map (fn (spec,name) => {spec=spec, decl=name}) list
  end


  (* note: result type of ptr MUST be a full ptr type *)
  fun convert_op_decl (attrOpt, spec, name, params) = let
    val l = case (attrOpt) of NONE => [] | SOME (l) => l
    val f = mapP flatten_operation_attr l
  in
    I.Oper {context = has_context_attr (f),
            exclude = has_exclude_attr (f),
            pre = has_pre_attr (f),
            post = has_post_attr (f),
            call = has_call_attr (f),
            spec = convert_type_spec (NONE,[],D.TS_Simple (spec)),
            params = map convert_param_declarator (convert_params params)}
  end

  fun convert_declaration (D.Type (attrOpt,spec,decls)) = let
        val attrs = case attrOpt of NONE => [] | SOME (ats) => ats
        val attrs = mapP flatten_type_attr attrs
        val ptr = extract_single_ptr_attr (attrs)
        val typ = extract_type_attrs (attrs)
        val sml_type = extract_sml_type (attrs)
        val spec' = (abstract_flag := false;
                     convert_type_spec (sml_type,typ,spec))
        val abstract = (has_abstract_attr (attrs)) orelse (!abstract_flag)
        val exclude = has_exclude_attr (attrs)
      in
        map (fn (x,name) => I.Type (name,I.TypeDef {spec=x,
                                                    exclude=exclude, (* was false, why? *)
                                                    abstract=abstract}))
            (explode_declarator (ptr,
                                 has_string_attr (attrs),
                                 NONE,spec',decls))
      end
    | convert_declaration (D.Const (spec,name,exp)) = let
        val const_val = convert_constant (spec,exp)
        val type_spec = convert_const_type_spec (spec)
      in
        [I.Constant (name,I.Const {value=const_val,
                                   spec=type_spec})]
      end
    | convert_declaration (D.Tagged t) = fail ["IdlTranslate.convert_declaration",
                                                     "Cannot convert Tagged declarations just yet"]
    (* eventually, may want to blow this up to a typedef with a suitable gensym'ed name *)
    | convert_declaration (D.Operation (oper as (_,_,name,_))) = let
        val oper = convert_op_decl oper
      in 
        [I.Operation (name,oper)]
      end

  fun translate (D.PT (l),file) = let
    (* initialize tagged table *)
    val _ = taggedTable := []
    val file_path = OS.Path.dir (file)
    val struct_name = ref NONE
    val sig_name = ref NONE
    val clib_name = ref NONE
    val clib_version = ref NONE
    val clib_date = ref NONE
    val cpp_quote = ref []
    fun convert_cpp_quote (s) = let
      fun translate [] = fail ["IdlTranslate.convert","convert_cpp_quote failure"]
        | translate [_] = []  (* remove trailing quote *)
        | translate (x::xs) =
            if (x = #"\n")
              then (#"\\")::(#"n")::translate (xs)
            else x::translate (xs)
    in
      String.implode (translate (case (String.explode (s))
                                   of [] => fail ["IdlTranslate.convert","convert_cpp_quote failure"]
                                    | x::xs => xs))
    end
    fun f (D.TopDecl (exp),decls) = (convert_declaration (exp))@decls
      | f (D.Import _,decls) = decls
      | f (D.StructName (s),decls) = (struct_name := SOME (Util.stripString (s)); decls)
      | f (D.SigName (s),decls) = (sig_name := SOME (Util.stripString (s)); decls)
      | f (D.CLibName (s),decls) = (clib_name := SOME (Util.stripString (s)); decls)
      | f (D.CLibVersion (s),decls) = (clib_version := SOME (Util.stripString (s)); decls)
      | f (D.CLibDate (s),decls) = (clib_date := SOME (Util.stripString (s)); decls)
      | f (D.CppQuote (s),decls) = (I.CppQuote (convert_cpp_quote (s)))::decls
    val decls = foldr f [] l 
    fun f (I.Type (name,td),st) = S.insert (st,name,td)
      | f (_,st) = st
    val st = foldl f S.empty decls
  in
    Verbose.message1 ["Translating IDL parse tree"];
    let 
      val i = I.IIL {symtable=st,
                      struct_name= !struct_name,
                      sig_name= !sig_name,
                      clib_name= !clib_name,
                      clib_version = !clib_version,
                      clib_date = !clib_date,
                      decls = decls}
    in
      Verbose.verbose (2,fn () => IIL.print (i)); i
    end
  end

end
