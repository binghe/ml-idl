(* generate-header-fn.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor GenerateHeaderFn (

    val fileExt : string	(* file extension *)
    val mlValueTy : string	(* ML value type name *)
    val mlStateTy : string	(* ML execution context type name *)
    val mlBasePath : string	(* path of base include file *)
    val target : string		(* target name; used by command-line *)

  ) : sig

    val generate : {
	    srcDir: string, srcFile: string, dstDir: string option,
	    spec: IIL.iil
	  } -> unit

  end = struct

    structure I = IIL
    structure A = Atom
    structure F = Format

    fun headerSym file = let
	  fun tr #"-" = "_"
	    | tr #"_" = "_"
	    | tr c = if Char.isAlphaNum c
		then str(Char.toUpper c)
		else F.format "_0x%02x_" [F.INT(ord c)]
	  val file' = String.translate tr file
	  in
	    concat["_", file', "_H_"]
	  end

    fun field2str (I.Fld{spec, name, ...}) =
	  type2str(Atom.toString name, spec)^";\n"

    and type2str (n, spec) = (case spec
           of I.TS_Id (a) => concat[Atom.toString a, " ", n]
            | I.TS_Void => "void "^n   (* this is only for result values of operations.... *)
            | I.TS_Real64 => "double "^n
            | I.TS_Real32 => "float "^n
            | I.TS_Int32 => "int "^n
            | I.TS_Int16 => "int "^n
            | I.TS_Int8 => "int "^n
            | I.TS_Word32 => "unsigned int "^n
            | I.TS_Word16 => "unsigned int "^n
            | I.TS_Word8 => "unsigned int "^n
            | I.TS_Bool => "int "^n
            | I.TS_String => "char *"^n
            | I.TS_Char => "char "^n
            | I.TS_Ref (spec) => type2str ("*"^n, spec)
            | I.TS_Ptr (spec) => type2str ("*"^n, spec)
            | I.TS_Option (spec) => type2str (n, spec)
            | I.TS_Struct (fl) =>
		concat ("struct {" :: ((map field2str fl) @ ["} ", n]))
            | I.TS_StructTag a =>  concat ["struct ", A.toString a, " ", n]
            | I.TS_Array {spec,size} => (
		Error.bug ["Arrays unsupported still..."];
        	"void *"^n)
            | I.TS_Sml (s) => concat[mlValueTy, " ", n]
            | I.TS_Int => "int "^n
            | I.TS_Word => "unsigned int "^n
            | _ => (
		Error.bug ["Unsupported type (most likely dependent)"];
        	"void *"^n)
	  (* end case *))

    fun exp2str (I.E_Int i) = F.format "%d" [F.LINT (Int32.toLarge i)]
      | exp2str (I.E_Word w) = F.format "%d" [F.LWORD w]
      | exp2str (I.E_Bool false) = "0"
      | exp2str (I.E_Bool true) = "1"
      | exp2str (I.E_String s) = concat["\"", String.toCString s, "\""]
      | exp2str (I.E_Char c) = concat["'", Char.toCString c, "'"]
      | exp2str (I.E_Id id) = Atom.toString id

    fun param2str (I.Prm{name, spec, ...}) = type2str (Atom.toString name, spec)

    fun generate {srcDir, srcFile, dstDir, spec} = let
	  val (I.IIL{decls, ...}) = spec
	  val file = Util.replace_extension (srcFile, "", fileExt)
	  val hsym = headerSym srcFile
	  fun gen outS = let
		fun pr s = TextIO.output(outS, s)
		fun prf (fmt, items) = pr(F.format fmt items)
		fun prl l = pr(String.concat l)
		fun genTypeDef (id, I.TypeDef{exclude=true, ...}) = ()
		  | genTypeDef (id, I.TypeDef{spec, ...}) =
		      prf("typedef %s;\n", [
			  F.STR(type2str(Atom.toString id, spec))
			])
		fun genConst (id, I.Const{value, ...}) =
		      prf("#define %s %s\n", [F.ATOM id, F.STR(exp2str value)])
		fun genOp (id, I.Oper{exclude=true,...}) = ()
                  | genOp (id, I.Oper{exclude=false,spec, params, context, ...}) =
		      prf ("extern %s (%s);\n", [
			  F.STR(type2str(Atom.toString id, spec)),
			  F.STR(let
			      val p = Util.concatSep (",", map param2str params)
                              in 
                                if context
				  then concat [
				      mlStateTy, " *msp",
				      case params of [] => "" | _ => ",",
				      p
				    ]
                                  else p
                              end)
			])
                (* this function is needed to convert \" -> " in the cpp_quote strings *)
                fun cppquote_decode s = let
                  fun loop []           acc = implode (rev acc)
                    | loop (#"\\" :: #"\"" :: cr) acc = loop cr (#"\"" :: acc)
                    | loop (c :: cr) acc = loop cr (c :: acc)
                in 
                  loop (explode s) [] 
                end
		fun genDcl (I.Type(id, def)) = genTypeDef(id, def)
		  | genDcl (I.Constant(id, con)) = genConst(id, con)
		  | genDcl (I.Operation(id, rator)) = genOp(id, rator)
		  | genDcl (I.CppQuote s) = prf ("%s\n",[F.STR (cppquote_decode (s))])
		in
		  prf ("#ifndef %s\n", [F.STR hsym]);
		  prf ("#define %s\n", [F.STR hsym]);
		  pr "\n";
		  prf ("#include \"%s\"\n", [F.STR mlBasePath]);
		  pr "\n";
		  List.app genDcl decls;
		  pr "\n";
		  prf ("#endif /* !%s */\n", [F.STR hsym])
		end
	  in
	    Verbose.message1 ["Generating header file in ", file];
	    Util.withOutputFile (Util.FILE_C, file, gen)
	  end

    val _ = CmdOptions.register (target, generate);

  end

structure GenerateCHeader = GenerateHeaderFn (

    val fileExt = "h"
    val mlValueTy = "ml_val_t"
    val mlStateTy = "ml_state_t"
    val mlBasePath = "ml-base.h"
    val target = "c-header"

  )

structure GenerateCXXHeader = GenerateHeaderFn (

    val fileExt = "hxx"
    val mlValueTy = "ML_Value"
    val mlStateTy = "ML_Context"
    val mlBasePath = "SMLNJ/base.h"
    val target = "c++-header"

  )
