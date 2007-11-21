(* generate-mbx.sml
 *
 * Generate MBX file from an IIL description
 * 
 * Note: this file will change slightly when the IIL interface is
 * rationalized
 *)

structure GenerateMBX : sig end = struct

  (*
   *   Abbreviations 
   *)

  structure I = IIL
  structure A = Atom
  structure S = AtomMap
  structure M = MBX
    
  exception Bug


  (* 
   *   Useful  functions 
   *)

  open Util

  val mapP = List.mapPartial

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  fun zip3 ([], [], []) = []
    | zip3 ((x::xs), (y::ys), (z::zs)) = (x,y,z) :: (zip3(xs, ys, zs))
    | zip3 _ = (Error.bug ["zip3: expected equal length strings."]; raise Bug)


  (*
   *   Symbol table for the current IIL translation
   *)

  val glob_symtable = ref (NONE) : I.typedef S.map option ref
  
  fun findType (I.TS_Id (s)) = 
    (case (!glob_symtable)
       of NONE => fail ["GenerateMBX.findType",
                        "uninitialized global symbol table"]
        | SOME (st) => 
            (case S.find (st,s)
               of SOME (I.TypeDef {spec,...}) => findType (spec)
                | NONE => Error.error ["is not a type"]))
    | findType (t) = t

  (* If we introduce BOL typedefs, then we need a symbol table
   * to track them
   *)

  val bol_symtable = ref (NONE) : MBX.bol_ty S.map option ref

  fun bol_saveTypeDef (s,bTy) =  (* called in genDTypeDef *)
    (case (!bol_symtable)
       of NONE => fail ["GenerateMBX.bol_saveTypeDef",
                        "uninitialized BOL symbol table"]
        | SOME (st) => (bol_symtable := SOME (S.insert (st,s,bTy))))

    local
      fun error s = raise Fail("unexpected named BOL type " ^ (Atom.toString s))
      fun lookup s = (case (!bol_symtable)
                        of NONE => fail ["GenerateMBX.lookup",
                                         "uninitialized BOL symbol table"]
                         | SOME (st) => (case (S.find (st,s))
                                           of NONE => error (s)
                                           | SOME (bTy) => bTy))
    in
    val wrapVars = MBXUtil.wrapVars lookup
    val unwrapVars = MBXUtil.unwrapVars lookup
    end

  (* 
   * take IIL structs apart
   * this should probably be folded into a IIL utility module
   *)

  (* align (p,a) = min{p' | p<=p' & p' mod a = 0}}} *)
  fun align (p,a) = let 
    val m = p mod a
  in
    if m=0 then p else p + (a - m)
  end

  (* compute the size of an IIL type *)
  (* actually, also returns the alignment information *)
  fun size (I.TS_Id (a)) = size (findType (I.TS_Id (a)))
    | size (I.TS_Void) = (0,0)
    | size (I.TS_Real64) = (8,8)
    | size (I.TS_Real32) = (4,4)
    | size (I.TS_Int32) = (4,4)
    | size (I.TS_Int16) = (2,4)
    | size (I.TS_Int8) = (1,4)
    | size (I.TS_Word32) = (4,4)
    | size (I.TS_Word16) = (2,4)
    | size (I.TS_Word8) = (1,4)
    | size (I.TS_Bool) = (1,4)
    | size (I.TS_String) = (4,4)
    | size (I.TS_Char) = (1,4)
    | size (I.TS_Ref _) = (4,4)
    | size (I.TS_Ptr _) = (4,4)
    | size (I.TS_Option t) = (4,4) (* can really only be a ptr *)
    | size (I.TS_Struct []) = (Error.bug ["Empty structure"]; raise Bug)
    | size (I.TS_Struct ((I.Fld {spec,...})::fl)) = let
        fun max (x,y) = if (x>=y) then x else y
        fun loop (I.Fld {spec,...},(next_p,max_a)) = let
          val (sz,a) = size (spec)
        in
          (align (next_p,a)+sz,max (a,max_a))
        end
        val (sz,a) = size (spec)
        val (next_p,max_a) = (align (0,a) + sz,a)
        val (next_p,max_a) = foldl loop (next_p,max_a) fl
      in
        (align (next_p,max_a),max_a)
      end
    | size _ = (Error.bug ["cannot compute size of type"]; (0,4))
      
    fun fieldInfo (fl) = let
      val sz = #1 (size (I.TS_Struct (fl)))
      val cTys = List.map (fn (I.Fld {spec,...}) => spec) fl
      val szAals = List.map size cTys
      val fldSzs    = List.map (fn (x,y) => x) szAals
      val fldAligns = List.map (fn (x,y) => y) szAals
      val sAlign =  List.foldr Int.max 0 fldAligns
      val offsets = let
        fun loop ((sz,a),(next_p,acc)) = let 
          val p = align (next_p,a)
        in
          (p+ sz,p::acc)
        end
      in
        case (szAals)
          of [] => (Error.bug ["Empty structure?"]; raise Bug)
           | (sz,a)::rest => rev (#2 (foldl loop (align (0,a) + sz,
                                                  [align (0,a)]) rest))
      end
    in
      {offsets = offsets,
       sizes   = fldSzs,
       types   = cTys,
       align   = sAlign,
       size    = sz}
    end
      

  (***********************************************************************
   *
   *    MBX
   * 
   *    Functions to create MBX entries
   *    Convert to both BOL and Moby types, and create types
   *    Some of these are snatched from Charon's ast-to-mbx.sml
   * 
   ***********************************************************************)


  (* convert IIL type to a BOL type *)
  (* the BOL type corresponds to the underlying C representation *)
  fun bolT (I.TS_Id (x)) = M.T_Named (x)
    | bolT (I.TS_Void) = M.T_Void
    | bolT (I.TS_Real64) = M.T_Double
    | bolT (I.TS_Real32) = M.T_Float
    | bolT (I.TS_Int32) = M.T_Int
    | bolT (I.TS_Int16) = M.T_Int
    | bolT (I.TS_Int8) = M.T_Int
    (* Signedness? *)
    | bolT (I.TS_Word32) = M.T_Int
    | bolT (I.TS_Word16) = M.T_Int
    | bolT (I.TS_Word8) = M.T_Int
    | bolT (I.TS_Bool) = M.T_Bool
    | bolT (I.TS_String) = M.T_Named (A.atom "c_string")
    | bolT (I.TS_Char) = M.T_Named (A.atom "char")
    | bolT (I.TS_Ref (t)) = M.T_Addr (bolT (t))
(*    | bolT (I.TS_Ptr (t)) = M.T_Ptr (bolT (t)) *)
    | bolT (I.TS_Option (t)) = M.T_Addr (bolT (t)) 
(* structs must be typedef'ed, until they can be defined directly *)
(*    | bolT (I.TS_Struct (fl)) =  *)
(*    | bolT (I.TS_StructTag (a)) =  ??? *)
    | bolT _ = (Error.bug ["unimplemented translation to BOL type"]; raise Bug)

  (* construct Moby types *)
  fun mkBaseTy(name) = MBX.NAMEDTY(MBX.BASETY([Atom.atom name]))
  fun mkFnTy(argTys, resTys) =
    MBX.TYSCHEME([], MBX.FNTY(argTys, resTys))
  fun mkTyApp(tyFnNm, bolTy) = 
    MBX.NAMEDTY(MBX.TYCON([Atom.atom tyFnNm], [bolTy]))
  fun nameT(name) = mkBaseTy(name)
    
  fun L mty = mkTyApp("LValue", mty)

  fun capitalize (s) = let
    val (first, rest) = Substring.splitAt(Substring.all(s), 1)
  in
    Substring.concat[Substring.all(String.str(Char.toUpper(Substring.sub(first, 0)))),
                     rest]
  end

  fun upcase (s) = String.map Char.toUpper s

  (* NOT SURE ABOUT THESE! Ask jhr *)
  (* translate an IIL type to a Moby type *)
  fun T (I.TS_Id (a)) = nameT (capitalize (A.toString (a)))
    | T (I.TS_Void) = nameT ("Unit")
    | T (I.TS_Real64) = nameT ("Double")
    | T (I.TS_Real32) = nameT ("Float")
    | T (I.TS_Int32) = nameT ("Int")
    | T (I.TS_Int16) = nameT ("Int")
    | T (I.TS_Int8) = nameT ("Int")
    (* ??? *)
    | T (I.TS_Word32) = nameT ("Int")
    | T (I.TS_Word16) = nameT ("Int")
    | T (I.TS_Word8) = nameT ("Int")
    | T (I.TS_String) = nameT ("String")
    | T (I.TS_Char) = nameT ("Char")
    | T (I.TS_Ref s) = T (s)  (* mkTyApp ("Ptr",T (s)) *)
    | T (I.TS_Option t) = mkTyApp ("Option",T (t))
    | T _ = (Error.bug ["cannot create Moby type"]; nameT "Unit")

  (* convert an IIL type to a BOL type corresponding to the Moby type
   * returned by T above. Again, ask jhr.
   *)
  fun bolMobyT (I.TS_Id (a)) = bolMobyT (findType (I.TS_Id (a)))
    | bolMobyT (I.TS_Void) = M.T_Void
    | bolMobyT (I.TS_Real64) = M.T_Double
    | bolMobyT (I.TS_Real32) = M.T_Float
    | bolMobyT (I.TS_Int32) = M.T_Int
    | bolMobyT (I.TS_Int16) = M.T_Int
    | bolMobyT (I.TS_Int8) = M.T_Int
    (* Signedness? *)
    | bolMobyT (I.TS_Word32) = M.T_Int
    | bolMobyT (I.TS_Word16) = M.T_Int
    | bolMobyT (I.TS_Word8) = M.T_Int
    | bolMobyT (I.TS_String) = M.T_Named (A.atom "string")
(*    | bolMobyT (I.TS_Char) = M.T_Named (A.atom "char")*)
    | bolMobyT (I.TS_Ref (t)) = bolMobyT (t)
    | bolMobyT (I.TS_Option (t)) = M.T_Named (A.atom "option")
    | bolMobyT (I.TS_Struct (fl)) = M.T_Any  (* ask jhr! --- what do tuples get represented as? *)
    | bolMobyT _ = (Error.bug ["unimplemented translation to BOL type"]; raise Bug)



  (* Declarations *)
  
  fun genDTypeDef(atom, bty) =  (bol_saveTypeDef (atom,bty);
                                 MBX.D_TYPEDEF(atom, bty))
      
  fun genITypeDecl(name, params) = 
    MBX.IM_TYPE(Atom.atom name, List.map Atom.atom params)


  (* Code *)
    
  fun mkBOLReturn(n) = MBX.BOL_Ret([MBX.BOL_TermVar(Atom.atom n)])
  fun mkBOLTerm(rator, args) = 
    MBX.BOL_Term(MBX.BOL_TermPrim(Atom.atom rator,args))
  fun mkBOLInt i = MBX.BOL_IConst(IntInf.fromInt i, MBX.T_Integer)
    

  (* Scaffolding *)

  (* stolen from Charon as is... should be used? *)
  val neededCTypes : string list ref = ref []
  val CBaseTypes = ["Char", "Short", "Int", "Long", "LongLong"]
  val CTypes = (List.map (fn s=> "U"^s) CBaseTypes) @
    (List.map (fn s=> "S"^s) CBaseTypes)
    
  (* import Moby basis stuff *)
  fun genBasis () = MBX.D_LIBRARY{
	    guid = "A5CEBED0-5446-11D3-B355-000502A6C462",
	    name = Atom.atom "moby-basis",
	    dcls = List.map genITypeDecl [
		 ("Int", []),
		 ("Long", []),
		 ("Integer", []),
		 ("Float", []),
		 ("Double", []),
		 ("Extended", []),
                             ("Char", []),
                             ("Option", []),
                             ("String",[])
	       ]
	  }

  fun genExn() = let
    val exn = Atom.atom "exn"
    val exnHandler = Atom.atom "exn_handler"
    val exnTy = MBX.T_Ptr(MBX.T_Data)
    val exnHandlerTy = MBX.T_ContPtr(MBX.T_Named exn)
  in
    [genDTypeDef(exn, exnTy),
     genDTypeDef(exnHandler, exnHandlerTy)]
  end

  val exhTy = MBX.T_Named(Atom.atom "exn_handler")


  fun genScaff(cdcls : MBX.decl list) = let
	val basisDcl = genBasis()
	val exnDcls = genExn()
	val dcls = (basisDcl :: exnDcls)
	in
	  MBX.F_LIBRARY{
(* FIXME: should be possible to get GUID from IDL file or command-line too! *)
	      guid = GUID.toString(GUID.uuidgen()),
	      dcls = dcls @ cdcls
	    }
	end


  (***********************************************************************
   *
   *   Convert IIL to MBX
   *
   ***********************************************************************)


  (*
   *    Type definitions
   *)

  fun make_def (a,I.TS_Struct (fl)) = let
    val n = A.toString (a)
    val specs = List.map (fn (I.Fld {spec,...}) => spec) fl

    fun structDecl(bolName, size, align, offsets, sizes, types) = let
      fun mkBolFld (offset,sz,cty) = let
        val bolTy = bolT (cty)
      in
        {offset = offset, mutable = true, sz = sz, 
         cnt = 1, ty=bolTy}
      end
      val data = List.map mkBolFld (zip3 (offsets,sizes,types))
      val bolTy = MBX.T_Struct{sz=size, align=align, data=data}
    in
      (* It seems that structures should always be addressed by reference  *)
      (* just return the typedef info.. don't construct it here *)
      (* MBX.D_TYPEDEF (A.atom bolName, MBX.T_Addr (bolTy)) *)
      (A.atom bolName, MBX.T_Addr (bolTy))
    end

    fun getStructInfo (fl) = let
      val (tyName, bolName, modName, size, align,offsets,sizes,types) = let
        val {offsets, sizes, types, align, size} = fieldInfo(fl)
      in
        (* naming issues! 
         * better approach: name every structure, generate for *that* name, 
         * and really do a typedef for the resulting structure 
         *)
        (capitalize (n),  (* "Struct_"^n,*)
         n,             (* "struct_"^n,*)
         "S"^n, 
         size, align, offsets, sizes, types)
      end
    in
      {tyName = tyName, bolName=bolName, modName=modName, 
       size=size, align=align, 
       offsets=offsets, sizes = sizes, types=types}
    end

    val {tyName,bolName,modName,size,align,offsets,sizes,types} = getStructInfo(fl)
    val tyDcl = MBX.D_DATATYPE (Atom.atom tyName, [], 
                                [(Atom.atom (upcase (tyName)),
                                  List.map T specs)])
    val (structName,structTy) = structDecl(bolName,size,align, 
                                           offsets,sizes,types) 
  in
    [tyDcl,genDTypeDef (structName,structTy)]
  end  
    | make_def (a,spec) = let
        val mTy = T (spec)
        val bTy = bolT (spec)
      in
        [M.D_TYPE (a,[],mTy),genDTypeDef (a,bTy)]
      end

  fun make_type_def (a,I.TypeDef {spec,exclude,...}) = 
    make_def (a,spec)



  (* 
   *   Operations
   *)

  (* Note: the functions below pass a 'bTy' around, which is an appropriate BOL type,
   * but the resulting code ends up using type inference in the %let bindings
   * (the bTy entry is used for the %stackalloc's)
   *)


  (* Construct MBX code for marshalling *)
  fun mkLet (exp,to,bTy,curr) = M.BOL_Let ([M.BOL_Var (to,M.T_Var (ref NONE))],
                                           exp, curr)
  fun mkSimpleLet (fr,to,bTy,curr) = mkLet (M.BOL_Term (M.BOL_TermVar (fr)),
                                            to,bTy,curr)
  fun mkStoreI32 (fr,to,curr) = 
    M.BOL_Let ([], 
               M.BOL_Term (M.BOL_TermPrim (A.atom "AdrStoreI32",
                                           [to, M.BOL_TermVar (fr)])),
               curr)

  fun mkLoadI32 (fr,to,bTy,curr) = 
    M.BOL_Let ([M.BOL_Var (to,M.T_Var (ref NONE))],
               M.BOL_Term (M.BOL_TermPrim (A.atom "AdrLoadI32",
                                           [fr])),
               curr)

  (* marshall the content of 'fr' into the address represent by term 'to' *)
  fun marshallAt (fr,to,I.TS_Id (x),curr) = marshallAt (fr,to,findType (I.TS_Id (x)),curr)
    | marshallAt (fr,to,I.TS_Int8,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Int16,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Int32,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Word8,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Word16,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Word32,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt (fr,to,I.TS_Bool,curr) = mkStoreI32 (fr,to,curr)
    | marshallAt _ = (Error.bug ["Unimplemented marshallAt for this type"]; raise Bug)

  (* marshall the content of 'fr' to variable 'to' *)
  fun marshall (fr,to,bTy,I.TS_Id (x),curr) = marshall (fr,to,bTy,findType (I.TS_Id (x)),curr)
    | marshall (fr,to,bTy,I.TS_Int8,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Int16,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Int32,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Word8,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Word16,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Word32,curr) = mkSimpleLet (fr,to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Bool,curr) = mkSimpleLet (fr,to,bTy,curr)
    (* to marshall a string, simply extract the data element *)
    | marshall (fr,to,bTy,I.TS_String,curr) = mkLet (M.BOL_Select (1,fr,bTy),to,bTy,curr)
    | marshall (fr,to,bTy,I.TS_Ref (I.TS_Id (x)),curr) = marshall (fr,to,bTy,I.TS_Ref (findType (I.TS_Id (x))),curr)
    | marshall (fr,to,bTy,ts as I.TS_Ref (I.TS_Struct (fields)),curr) = let
        val (sz,al) = size (I.TS_Struct (fields))
        val {offsets,...} = fieldInfo (fields)
        (* ask jhr: Index for tuples starts at 0 or at 1? *)
        val fields' = zip3 (fields, offsets,List.tabulate (length (fields),fn i => i))
        fun loop ((I.Fld {name,spec,...},offset,index),curr) = let
          val to' = (case offset
                       of 0 => M.BOL_TermVar (to)
                        | i => M.BOL_TermPrim (A.atom "AdrAdd",
                                               [M.BOL_TermVar (to),
                                                M.BOL_IConst (IntInf.fromInt (i),
                                                              M.T_Int)]))
          val tmp = MBXUtil.genVar (SOME to)
          val bTy = bolT (spec)
        in
          M.BOL_Let ([M.BOL_Var (tmp,M.T_Var (ref NONE))],
                     M.BOL_Select (index,fr,bTy),
                     marshallAt (tmp,to',spec,curr))
        end
        val marshalledFields = List.foldr loop curr fields'
      in
        M.BOL_StackAlloc (M.BOL_Var (to,bTy), sz,al,marshalledFields)
      end
    | marshall (fr,to,bTy,I.TS_Ref (ts),curr) = let
        val (sz,al) = size (ts)
      in
        M.BOL_StackAlloc (M.BOL_Var (to,bTy),sz,al,
                          marshallAt (fr,M.BOL_TermVar (to),ts,curr))
      end
    | marshall (fr,to,bTy,ts,curr) = (Error.bug ["cannot marshall type"]; raise Bug)


  fun unmarshallFrom (fr,to,bTy,I.TS_Id (x),curr) = unmarshallFrom (fr,to,bTy,findType (I.TS_Id (x)),curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Int8, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Int16, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Int32, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Word8, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Word16, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Word32, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,I.TS_Bool, curr) = mkLoadI32 (fr,to,bTy,curr)
    | unmarshallFrom (fr,to,bTy,ts,curr) = (Error.bug ["cannot unmarshallFrom type"]; raise Bug)


  (* unmarshall a value in 'fr' into a value in 'to' *)
  fun unmarshall (fr,to,bTy,I.TS_Id (x),curr) = unmarshall (fr,to,bTy,findType (I.TS_Id (x)),curr)
    | unmarshall (fr,to,bTy,I.TS_Int8,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Int16,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Int32,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Word8,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Word16,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Word32,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_Bool,curr) = mkSimpleLet (fr,to,bTy,curr)
    | unmarshall (fr,to,bTy,I.TS_String,curr) = let
        val tmp_len = MBXUtil.genVar (SOME to)
        val tmp_res = MBXUtil.genVar (SOME to)
      in
        M.BOL_Let ([M.BOL_Var (tmp_len,M.T_Var (ref NONE))],
                   M.BOL_CCall (A.atom "strlen",[M.BOL_TermVar (fr)]),
                   M.BOL_Let ([M.BOL_Var (tmp_res,M.T_Var (ref NONE))],
                              M.BOL_Alloc ([(false,M.BOL_TermVar (tmp_len)),
                                            (false,M.BOL_TermVar (fr))]),
                              M.BOL_Let ([M.BOL_Var (to,M.T_Var (ref NONE))],
                                         M.BOL_Alloc ([(false,M.BOL_TermVar (tmp_res))]),
                                         curr)))
      end
    | unmarshall (fr,to,bTy,I.TS_Ref (I.TS_Id (x)),curr) = unmarshall (fr,to,bTy,I.TS_Ref (findType (I.TS_Id (x))),curr)    
    | unmarshall (fr,to,bTy,ts as I.TS_Ref (I.TS_Struct (fields)),curr) = let
        val (sz,al) = size (I.TS_Struct (fields))
        val {offsets,...} = fieldInfo (fields)
        val fields' = zip3 (fields, offsets,List.tabulate (length (fields),
                                                           fn _ => MBXUtil.genVar (SOME fr)))
        fun loop ((I.Fld {name,spec,...},offset,tmp),curr) = let
          val fr' = (case offset
                       of 0 => M.BOL_TermVar (fr)
                        | i => M.BOL_TermPrim (A.atom "AdrAdd",
                                               [M.BOL_TermVar (fr),
                                                M.BOL_IConst (IntInf.fromInt (i),
                                                              M.T_Int)]))
          val bTy = bolT (spec)
        in
          unmarshallFrom (fr',tmp,bTy,spec,curr)
        end
        (* Immutable fields or not? I presume immutable *)
        val accumulateAll = 
          M.BOL_Let ([M.BOL_Var (to,M.T_Var (ref NONE))],
                     M.BOL_Alloc (List.map (fn (_,_,tmp) => (false,M.BOL_TermVar (tmp))) fields'),
                     curr)
      in
        List.foldr loop accumulateAll fields'
      end
    | unmarshall (fr,to,bTy, I.TS_Ref (ts),curr) = unmarshallFrom (M.BOL_TermVar (fr),to,bTy,ts,curr)
    | unmarshall (fr,to,bTy, I.TS_Option (t),curr) = 
      M.BOL_If (M.BOL_TermPrim (A.atom "AdrEq",[M.BOL_TermVar (fr),M.BOL_nil (M.T_Var (ref NONE))]),
                M.BOL_Let ([M.BOL_Var (to,M.T_Var (ref NONE))],
                           M.BOL_Term (M.BOL_IConst (IntInf.fromInt (0),M.T_Named (A.atom "option"))),
                           curr),
                unmarshall (fr,to,bTy,t,curr))
    | unmarshall (fr,to,bTy, ts,curr) = (Error.bug ["Cannot unmarshall type"]; raise Bug)

  fun make_operation (name,oper as I.Oper {spec,params,...}) = let
    val n = A.toString name
    val in_params = List.filter (fn (I.Prm {dir,...}) =>
                                 I.has_In (dir)) params
    val out_params = List.filter (fn (I.Prm {dir,...}) =>
                                  I.has_Out (dir)) params
    fun specs l = map (fn (I.Prm {spec,...}) => spec) l
    (* external declaration *)
    val retTy =  bolT (spec)
    val argTys = map bolT (specs params)
    val externDecl = M.D_CFUN ([],retTy,name,argTys)
    (* function declaration *)
    val bolCode = let
      fun genVar s = MBXUtil.genVar(SOME(Atom.atom s))
      (* first entry is the (name,ty) pair,
       * second entry is the BOL_Var with the "marsh_" *)
      fun marshName (s) = A.atom ("marsh_"^(A.toString (s)))
      val (res,resVar) = 
        (case retTy
           of M.T_Void => ([],[])
            | ty => let 
                val n = genVar (n^"_result")
              in
                ([(n,ty)],[M.BOL_Var (marshName (n),M.T_Var (ref NONE))])
              end)
      (* args,in_args,out_args are lists of pairs, one for each argument (in, or out, resp.)
       * each pair contains the name of the argument, and the bol type corresponding to the 
       * Moby type passed to the function --- not the bol type corresponding to the underlying
       * ccall. 
       *)
      val args = List.map (fn (I.Prm {name,spec,...}) => (name,bolMobyT (spec))) params
      val in_args = List.map (fn (I.Prm {name,spec,...}) => (name,bolMobyT (spec))) in_params
      val out_args = res@(List.map (fn (I.Prm {name,spec,...}) => (name,bolMobyT (spec))) out_params)
      val retExp = wrapVars {
                     vars = out_args,
                     mkBody =
                     fn vl => MBX.BOL_Ret(List.map MBX.BOL_TermVar vl)
                   }
      val unmarshallExp = let
        val lastExp = (case res
                         of [] => retExp
                          | [(n,ty)] => unmarshall (marshName (n),n,ty,spec,retExp))
        fun loop (I.Prm {name,spec,...},curr) = 
          unmarshall (marshName (name),name,bolT(spec),spec,curr)
      in
        List.foldr loop lastExp out_params
      end
      val ccall = M.BOL_Let(resVar,
                            M.BOL_CCall(name, List.map (M.BOL_TermVar o marshName o #1) args),
                            unmarshallExp)
      (* for now, marshall even the stuff that does not need marshalling
       * optimize later *)
      val marshallExp = let
        fun loop (I.Prm {name,spec,...},curr) = 
          marshall (name,marshName (name),bolT (spec),spec,curr)
      in
        List.foldr loop ccall in_params
      end
      val stackalloc = let
        fun loop (I.Prm {name,spec,...},curr) = let
          val spec' = (case spec
                        of I.TS_Ref (ts) => ts
                          | _ => (Error.bug ["Non-ref out parameter in stack allocation"];
                                  raise Bug))
          val (sz,al) = size (spec')
        in
          M.BOL_StackAlloc (M.BOL_Var (marshName (name),bolT (spec)),
                            sz,al,curr)
        end
      in
        List.foldr loop marshallExp out_params
      end
      val (params, body) = unwrapVars{vars=in_args, body=stackalloc}
      val params = (List.map M.BOL_Var params) @ [M.BOL_AnonVar exhTy]
    in
      M.LAMBDA(genVar n, params, retTy, body)
    end
    val argMTys = map T (specs (in_params))
    val resMTy = let
      val t = map T (specs (out_params))
    in
      case (spec)
        of I.TS_Void => t
         | _ => (T (spec))::t
    end
    val funMTy = M.TYSCHEME ([],M.FNTY (argMTys,resMTy))
    (* what name do we give the function? This may well be incorrect... ask jhr*)
    val funDecl = M.D_VAL (name,funMTy,M.VAL_PrimFn (bolCode))
  in
    [externDecl,funDecl]
  end





  (*
   *    Constants
   *)
  fun make_const (a,I.Const {value,spec,...}) = 
    (Error.bug ["const declarations not yet supported"];
     raise Bug)
    


  (*
   *    Entry point (generate function)
   *)
  fun generate {srcDir, srcFile, dstDir, spec} = let
	val I.IIL {symtable,decls,...} = spec
      (* These should probably only be generated on-demand *)
	fun genBasicBol () = [ 
		genDTypeDef (A.atom "char", M.T_Enum{lo=0w0, hi=0w255}),
		genDTypeDef (A.atom "string_data", 
                	     M.T_Ptr (M.T_Vector{len=NONE,
                                        	  elemSz=1,
                                        	  ty= M.T_Named(A.atom "char")})),
		genDTypeDef (A.atom "string",
                	     M.T_Ptr (M.T_Struct {sz=8,align=4,
                                        	  data=[{offset=0,mutable=false,sz=4,cnt=1,
                                                	 ty=M.T_Int},
                                        		{offset=4,mutable=false,sz=4,cnt=1,
                                                	 ty=M.T_Named (A.atom "string_data")}]})),
		genDTypeDef (A.atom "option",
                	     M.T_PtrOrEnum {ptrTy=M.T_Struct {sz=4,align=4,
                                                	      data=[{offset=0,mutable=false,sz=4,cnt=1,
                                                        	     ty=M.T_Any}]},
                                	    enumTy = M.T_Enum {lo=0w0,hi=0w0}}),
		genDTypeDef (A.atom "c_string",
                	     M.T_Addr (M.T_Vector {len=NONE,elemSz=1,ty=M.T_Named(A.atom "char")})),
		M.D_CFUN ([],M.T_Int,A.atom "strlen",[M.T_Named (A.atom "c_string")])
	      ]
	fun makeMBX () = let
	    (* this needs to go here, to make sure the symbol table is initialized *)
	      val bolDecls = genBasicBol ()
	      fun make_decl (I.Type (name,td)) = (make_type_def (name,td) handle Bug => [])
        	| make_decl (I.Constant (name,c)) = (make_const (name,c) handle Bug => [])
        	| make_decl (I.Operation (name,oper)) = (make_operation (name,oper) handle Bug => [])
        	| make_decl _ = (Error.bug ["unsupported declaration type"]; [])
	      in
		genScaff (bolDecls @ (List.concat (map make_decl decls)))
	      end
	val fmbx = replace_extension (srcFile, "", "mbx")
	fun ppMBX (fname, mbx) = let 
	      val outFile = TextIO.openOut (fname)
	      val ppStrm = TextIOPP.openOut{dst = outFile, wid = 80} 
	      in
        	MBXPP.ppFile(ppStrm, mbx);
        	TextIOPP.closeStream ppStrm;
        	TextIO.closeOut outFile
	      end
	in
	  Verbose.message1 ["Generating MBX code"];
	  glob_symtable := SOME (symtable);
	  bol_symtable := SOME (S.empty);
	  ppMBX (fmbx, makeMBX ())
	end 

end

