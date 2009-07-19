functor IdlLexFun (structure Tokens : Idl_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT0 | COMMENT1 | INITIAL
    structure UserDeclarations = 
      struct

(* idl.lex
 * 
 * ML-Lex description file for IDL.
 * Originally generated from a Fred description of the grammar.
 *)


structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

structure Keywords = struct
  type pos = int
  type token = (svalue,pos) Tokens.token
  fun ident (a,p1,p2) = Tokens.ID (a,p1,p2)
  fun mkKW (kw,tk) = (kw,fn (p1:pos,p2:pos) => tk (p1,p2))
  val keywords = map mkKW [
                           (*  ("import", Tokens.KW_import), *)
                           ("const", Tokens.KW_const),
                           ("typedef", Tokens.KW_typedef),
                           ("switch", Tokens.KW_switch),
                           ("case", Tokens.KW_case),
                           ("default", Tokens.KW_default),
                           ("enum", Tokens.KW_enum),
                           ("struct", Tokens.KW_struct),
                           ("union", Tokens.KW_union),
                           ("float", Tokens.KW_float),
                           ("double", Tokens.KW_double),
                           ("hyper", Tokens.KW_hyper),
                           ("unsigned", Tokens.KW_unsigned),
                           ("int", Tokens.KW_int),
                           ("long", Tokens.KW_long),
                           ("short", Tokens.KW_short),
                           ("small", Tokens.KW_small),
                           ("char", Tokens.KW_char),
                           ("boolean", Tokens.KW_boolean),
                           ("byte", Tokens.KW_byte),
                           ("void", Tokens.KW_void),
(*                           ("wchar_t", Tokens.KW_wchar_t), *)
                           ("TRUE", Tokens.KW_TRUE),
                           ("FALSE", Tokens.KW_FALSE),
                           ("NULL", Tokens.KW_NULL),
                           ("string", Tokens.KW_string),
                           ("size_is", Tokens.KW_size_is),
                           ("ref", Tokens.KW_ref),
                           ("unique", Tokens.KW_unique),
                           ("ptr", Tokens.KW_ptr),
                           ("in", Tokens.KW_in),
                           ("out", Tokens.KW_out),
                           ("sml_structure", Tokens.KW_sml_structure),
                           ("sml_signature", Tokens.KW_sml_signature),
                           ("clib_name", Tokens.KW_clib_name),
                           ("clib_date", Tokens.KW_clib_date),
                           ("clib_version", Tokens.KW_clib_version),
                           ("sml_type", Tokens.KW_sml_type),
			   ("cpp_type", Tokens.KW_cpp_type),
                           ("sml_value", Tokens.KW_sml_value),
                           ("sml_context", Tokens.KW_sml_context),
                           ("exclude", Tokens.KW_exclude),
                           ("abstract", Tokens.KW_abstract),
                           ("cpp_quote", Tokens.KW_cpp_quote),
                           ("pre", Tokens.KW_pre),
                           ("post", Tokens.KW_post),
                           ("call", Tokens.KW_call),
                           ("sml_int", Tokens.KW_sml_int),
                           ("sml_word", Tokens.KW_sml_word)
(*                           ("error_status_t", Tokens.KW_error_status_t),
                           ("ISO_LATIN1", Tokens.KW_ISO_LATIN1),
                           ("ISO_MULTILINGUAL", Tokens.KW_ISO_MULTILINGUAL),
                           ("ISO_UCS", Tokens.KW_ISO_UCS),
                           ("idempotent", Tokens.KW_idempotent),
                           ("broadcast", Tokens.KW_broadcast),
                           ("maybe", Tokens.KW_maybe),
                           ("reflect_deletions", Tokens.KW_reflect_deletions),
                           ("ignore", Tokens.KW_ignore),
                           ("context_handle", Tokens.KW_context_handle),
                           ("first_is", Tokens.KW_first_is),
                           ("last_is", Tokens.KW_last_is),
                           ("length_is", Tokens.KW_length_is),
                           ("min_is", Tokens.KW_min_is),
                           ("max_is", Tokens.KW_max_is),
                           ("transmit_as", Tokens.KW_transmit_as),
                           ("handle", Tokens.KW_handle),
                           ("switch_type", Tokens.KW_switch_type),
                           ("switch_is", Tokens.KW_switch_is),
                           ("handle_t", Tokens.KW_handle_t) *) 
                           ]
end

structure K = KeywordFn (Keywords)

(*
 * line number
 *)
val pos = ref 1

fun error (s,i : int,_) = raise IdlExn.Lexer (concat [s," [line ",(Int.toString i),"]"])

fun eof () = Tokens.EOF (!pos,!pos)

val comment0 = ref (0)

val comment1 = ref (0)



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos)+1;continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.STRING (yytext,!pos,!pos))
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.INT32 (case Int32.fromString (yytext)
                    of NONE => error ("cannot convert int32",!pos,!pos) 
                     | SOME (v) => v,!pos,!pos))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.WORD32 (case Word32.fromString (yytext) 
                     of NONE => error ("cannot convert word32",!pos,!pos) 
                      | SOME (v) => v,!pos,!pos))
      end
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.UUID (yytext,!pos,!pos))
      end
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DDOT (!pos,!pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT (!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA (!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN (!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN (!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACK (!pos,!pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACK (!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE (!pos,!pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE (!pos,!pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI (!pos,!pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON (!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ASTERISK (yytext,!pos,!pos))
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.QUOTE (!pos,!pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQSIGN (!pos,!pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DASH (!pos,!pos)))
fun yyAction21 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (K.keyword (yytext,!pos,!pos))
      end
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT0; 
    continue ()))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL;
    pos := (!pos)+1;
    continue()))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT1; 
    comment1 := 1; 
    continue ()))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (comment1 := (!comment1)+1;
    continue ()))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos)+1; 
    continue ()))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (comment1 := (!comment1)-1;
    if (!comment1)=0 
      then (YYBEGIN INITIAL;
            continue ())
    else continue ()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (error ("Bad character "^yytext,!pos,!pos))
      end
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ38(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"0"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ38(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ38(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"0"
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ38(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction21(strm, yyNO_MATCH)
                      else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ48(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ48(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ48(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ48(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ48(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"."
                  then yystuck(lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ47(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ46(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ46(strm', lastMatch)
            else if inp = #"a"
              then yyQ46(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ46(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ46(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ46(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"."
                  then yystuck(lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ45(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ44(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ44(strm', lastMatch)
            else if inp = #"a"
              then yyQ44(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ44(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ44(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ44(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ44(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ44(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ44(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"."
                  then yystuck(lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ43(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = #"a"
              then yyQ42(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ42(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ42(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ42(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ42(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ42(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"G"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"G"
              then if inp = #"0"
                  then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ40(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"g"
              then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"G"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"G"
              then if inp = #"0"
                  then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ40(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction21(strm, yyNO_MATCH)
                  else yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"g"
              then yyQ41(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ39(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #":"
              then yystuck(lastMatch)
            else if inp < #":"
              then if inp = #"."
                  then yystuck(lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ40(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp <= #"/"
                  then yystuck(lastMatch)
                  else yyQ50(strm', lastMatch)
            else if inp = #"G"
              then yystuck(lastMatch)
            else if inp < #"G"
              then if inp <= #"@"
                  then yystuck(lastMatch)
                  else yyQ50(strm', lastMatch)
            else if inp = #"a"
              then yyQ50(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ50(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ49(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"G"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"G"
              then if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"."
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ49(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"G"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"G"
              then if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"f"
              then yyQ53(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ53(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ53(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ53(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"a"
              then yyQ53(strm', lastMatch)
            else if inp < #"a"
              then if inp <= #"F"
                  then yyQ53(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"f"
              then yyQ53(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"x"
              then yyQ52(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"."
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ40(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ49(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ49(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"g"
              then if inp = #"G"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"G"
                  then yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"w"
              then yyQ51(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"+"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #"+"
              then if inp = #"*"
                  then yyQ54(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"/"
              then yyQ55(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ56(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ57(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ60(strm', lastMatch)
                  else yyQ57(strm', lastMatch)
            else if inp = #"\\"
              then yyQ59(strm', lastMatch)
              else yyQ57(strm', lastMatch)
      (* end case *))
and yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ57(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ58(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyQ57(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ59(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyQ57(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
      (* end case *))
and yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ57(strm', lastMatch)
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ58(strm', lastMatch)
                  else yyQ57(strm', lastMatch)
            else if inp = #"\\"
              then yyQ59(strm', lastMatch)
              else yyQ57(strm', lastMatch)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ57(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #"\""
                  then yyQ58(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyQ57(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"\\"
              then yyQ59(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyQ57(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ15(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ61(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ61(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ61(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ61(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ27(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"'"
                  then yyQ18(strm', lastMatch)
                else if inp < #"'"
                  then if inp = #"\^N"
                      then yyQ13(strm', lastMatch)
                    else if inp < #"\^N"
                      then if inp = #"\n"
                          then yyQ15(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ14(strm', lastMatch)
                              else yyQ13(strm', lastMatch)
                        else if inp = #"\r"
                          then yyQ16(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = #"!"
                      then yyQ13(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #" "
                          then yyQ14(strm', lastMatch)
                          else yyQ13(strm', lastMatch)
                    else if inp = #"\""
                      then yyQ17(strm', lastMatch)
                      else yyQ13(strm', lastMatch)
                else if inp = #","
                  then yyQ22(strm', lastMatch)
                else if inp < #","
                  then if inp = #"*"
                      then yyQ21(strm', lastMatch)
                    else if inp < #"*"
                      then if inp = #"("
                          then yyQ19(strm', lastMatch)
                          else yyQ20(strm', lastMatch)
                      else yyQ13(strm', lastMatch)
                else if inp = #"/"
                  then yyQ25(strm', lastMatch)
                else if inp < #"/"
                  then if inp = #"-"
                      then yyQ23(strm', lastMatch)
                      else yyQ24(strm', lastMatch)
                  else yyQ26(strm', lastMatch)
            else if inp = #"]"
              then yyQ34(strm', lastMatch)
            else if inp < #"]"
              then if inp = #">"
                  then yyQ13(strm', lastMatch)
                else if inp < #">"
                  then if inp = #";"
                      then yyQ29(strm', lastMatch)
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ28(strm', lastMatch)
                          else yyQ27(strm', lastMatch)
                    else if inp = #"<"
                      then yyQ13(strm', lastMatch)
                      else yyQ30(strm', lastMatch)
                else if inp = #"G"
                  then yyQ32(strm', lastMatch)
                else if inp < #"G"
                  then if inp <= #"@"
                      then yyQ13(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = #"["
                  then yyQ33(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ13(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = #"{"
              then yyQ35(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"`"
                  then yyQ13(strm', lastMatch)
                else if inp < #"`"
                  then if inp = #"^"
                      then yyQ13(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp <= #"f"
                  then yyQ31(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = #"~"
              then yyQ37(strm', lastMatch)
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ13(strm', lastMatch)
                  else yyQ36(strm', lastMatch)
              else yyQ13(strm', lastMatch)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ11(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ12(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ7(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yyQ6(strm', lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yyQ6(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ7(strm', lastMatch)
                      else yyQ6(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ8(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = #"+"
              then yyQ6(strm', lastMatch)
            else if inp < #"+"
              then if inp = #"*"
                  then yyQ9(strm', lastMatch)
                  else yyQ6(strm', lastMatch)
            else if inp = #"/"
              then yyQ10(strm', lastMatch)
              else yyQ6(strm', lastMatch)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ4(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ3(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ4(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"\r"
              then yyQ5(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT0 => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT1 => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
