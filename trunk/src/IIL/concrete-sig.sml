(* concrete-sig.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * Signature for concrete mappings
 *
 *)

signature CONCRETE_SIG = sig

  val name : string
  val supportFiles : IIL.iil -> string list
  val init : IIL.iil -> string
  val oper : string * IIL.type_spec * IIL.param list -> string
  val call : string * IIL.type_spec * IIL.param list -> string
  val query : (IIL.type_spec * string) -> string

end
