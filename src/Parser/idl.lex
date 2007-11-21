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

%%

%s COMMENT0 COMMENT1;
%header (functor IdlLexFun (structure Tokens : Idl_TOKENS));

eol=("\n"|"\013\n"|"\013");
alpha=[A-Za-z];
digit=[0-9];
hexdigit=[0-9a-fA-F];
ws=[\ \t];
firstchar=({alpha}|"_");
allchar=({firstchar}|{digit});
identifier={firstchar}{allchar}*;

%%

<INITIAL>{eol} => 
   (pos := (!pos)+1;continue());
<INITIAL>{ws}+ => 
   (continue());
<INITIAL>\"([^"]|\\\")*\" => 
   (Tokens.STRING (yytext,!pos,!pos));
<INITIAL>(~)?[0-9]+ => 
   (Tokens.INT32 (case Int32.fromString (yytext)
                    of NONE => error ("cannot convert int32",!pos,!pos) 
                     | SOME (v) => v,!pos,!pos));
<INITIAL>"0wx"[0-9a-fA-F]+ => 
   (Tokens.WORD32 (case Word32.fromString (yytext) 
                     of NONE => error ("cannot convert word32",!pos,!pos) 
                      | SOME (v) => v,!pos,!pos));
<INITIAL>[0-9a-fA-F]+"-"[0-9a-fA-F]+"-"[0-9a-fA-F]+"-"[0-9a-fA-F]+"-"[0-9a-fA-F]+ => 
   (Tokens.UUID (yytext,!pos,!pos));
<INITIAL>".." => 
   (Tokens.DDOT (!pos,!pos));
<INITIAL>"." => 
   (Tokens.DOT (!pos,!pos));
<INITIAL>"," => 
   (Tokens.COMMA (!pos,!pos));
<INITIAL>"(" => 
   (Tokens.LPAREN (!pos,!pos));
<INITIAL>")" => 
   (Tokens.RPAREN (!pos,!pos));
<INITIAL>"[" => 
   (Tokens.LBRACK (!pos,!pos));
<INITIAL>"]" => 
   (Tokens.RBRACK (!pos,!pos));
<INITIAL>"{" => 
   (Tokens.LBRACE (!pos,!pos));
<INITIAL>"}" => 
   (Tokens.RBRACE (!pos,!pos));
<INITIAL>";" => 
   (Tokens.SEMI (!pos,!pos));
<INITIAL>":" => 
   (Tokens.COLON (!pos,!pos));
<INITIAL>"*" => 
  (Tokens.ASTERISK (yytext,!pos,!pos));
<INITIAL>"'" => 
   (Tokens.QUOTE (!pos,!pos));
<INITIAL>"=" => 
   (Tokens.EQSIGN (!pos,!pos));
<INITIAL>"-" => 
   (Tokens.DASH (!pos,!pos));
<INITIAL>{identifier} => 
   (K.keyword (yytext,!pos,!pos));
<INITIAL>"//" => 
   (YYBEGIN COMMENT0; 
    continue ());
<COMMENT0>{eol} => 
   (YYBEGIN INITIAL;
    pos := (!pos)+1;
    continue());
<COMMENT0>. => 
   (continue ());
<INITIAL>"/*" => 
   (YYBEGIN COMMENT1; 
    comment1 := 1; 
    continue ());
<COMMENT1>"/*" => 
   (comment1 := (!comment1)+1;
    continue ());
<COMMENT1>{eol} => 
   (pos := (!pos)+1; 
    continue ());
<COMMENT1>"*/" => 
   (comment1 := (!comment1)-1;
    if (!comment1)=0 
      then (YYBEGIN INITIAL;
            continue ())
    else continue ());
<COMMENT1>. => 
   (continue ());
. => 
   (error ("Bad character "^yytext,!pos,!pos));
