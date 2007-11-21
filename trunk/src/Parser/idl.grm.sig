signature Idl_TOKENS =
sig
type ('a,'b) token
type svalue
val KW_switch_is:  'a * 'a -> (svalue,'a) token
val KW_switch_type:  'a * 'a -> (svalue,'a) token
val KW_sml_word:  'a * 'a -> (svalue,'a) token
val KW_sml_int:  'a * 'a -> (svalue,'a) token
val KW_call:  'a * 'a -> (svalue,'a) token
val KW_post:  'a * 'a -> (svalue,'a) token
val KW_pre:  'a * 'a -> (svalue,'a) token
val KW_cpp_quote:  'a * 'a -> (svalue,'a) token
val KW_abstract:  'a * 'a -> (svalue,'a) token
val KW_exclude:  'a * 'a -> (svalue,'a) token
val KW_sml_context:  'a * 'a -> (svalue,'a) token
val KW_sml_value:  'a * 'a -> (svalue,'a) token
val KW_sml_type:  'a * 'a -> (svalue,'a) token
val KW_clib_version:  'a * 'a -> (svalue,'a) token
val KW_clib_date:  'a * 'a -> (svalue,'a) token
val KW_clib_name:  'a * 'a -> (svalue,'a) token
val KW_sml_signature:  'a * 'a -> (svalue,'a) token
val KW_sml_structure:  'a * 'a -> (svalue,'a) token
val KW_out:  'a * 'a -> (svalue,'a) token
val KW_in:  'a * 'a -> (svalue,'a) token
val KW_ptr:  'a * 'a -> (svalue,'a) token
val KW_unique:  'a * 'a -> (svalue,'a) token
val KW_ref:  'a * 'a -> (svalue,'a) token
val KW_size_is:  'a * 'a -> (svalue,'a) token
val KW_string:  'a * 'a -> (svalue,'a) token
val KW_NULL:  'a * 'a -> (svalue,'a) token
val KW_FALSE:  'a * 'a -> (svalue,'a) token
val KW_TRUE:  'a * 'a -> (svalue,'a) token
val KW_void:  'a * 'a -> (svalue,'a) token
val KW_byte:  'a * 'a -> (svalue,'a) token
val KW_boolean:  'a * 'a -> (svalue,'a) token
val KW_char:  'a * 'a -> (svalue,'a) token
val KW_small:  'a * 'a -> (svalue,'a) token
val KW_short:  'a * 'a -> (svalue,'a) token
val KW_long:  'a * 'a -> (svalue,'a) token
val KW_int:  'a * 'a -> (svalue,'a) token
val KW_unsigned:  'a * 'a -> (svalue,'a) token
val KW_hyper:  'a * 'a -> (svalue,'a) token
val KW_double:  'a * 'a -> (svalue,'a) token
val KW_float:  'a * 'a -> (svalue,'a) token
val KW_union:  'a * 'a -> (svalue,'a) token
val KW_struct:  'a * 'a -> (svalue,'a) token
val KW_enum:  'a * 'a -> (svalue,'a) token
val KW_default:  'a * 'a -> (svalue,'a) token
val KW_case:  'a * 'a -> (svalue,'a) token
val KW_switch:  'a * 'a -> (svalue,'a) token
val KW_typedef:  'a * 'a -> (svalue,'a) token
val KW_const:  'a * 'a -> (svalue,'a) token
val DASH:  'a * 'a -> (svalue,'a) token
val EQSIGN:  'a * 'a -> (svalue,'a) token
val QUOTE:  'a * 'a -> (svalue,'a) token
val ASTERISK: (string) *  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val DDOT:  'a * 'a -> (svalue,'a) token
val UUID: (string) *  'a * 'a -> (svalue,'a) token
val WORD32: (Word32.word) *  'a * 'a -> (svalue,'a) token
val INT32: (Int32.int) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val ID: (Atom.atom) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Idl_LRVALS=
sig
structure Tokens : Idl_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
