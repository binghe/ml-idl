(* cmd-options.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

structure CmdOptions : sig

  (* all generators should match the following interface *)
    type gen_act = {
	srcDir : string,
	srcFile : string,
	dstDir : string option,
	spec : IIL.iil
      } -> unit

    val register : (string * gen_act) -> unit

    datatype option
      = HelpOpt
      | VerboseOpt of int
      | TargetOpt of gen_act

    val usage : unit -> string

    val doOpts : string list -> (option list * string list)

  end = struct

    structure G = GetOpt

    type gen_act = {
	srcDir : string,
	srcFile : string,
	dstDir : string option,
	spec : IIL.iil
      } -> unit

    datatype option
      = HelpOpt
      | VerboseOpt of int
      | TargetOpt of gen_act

    val targets = ref([] : (string * gen_act) list)

    fun register tgt = (
	  print(concat["registering ", #1 tgt, "\n"]);
	  targets := tgt :: !targets)

    val header = "Usage: ml-idl [OPTION...] file"

    fun getTarget s = (case List.find (fn (r, _) => (s = r)) (!targets)
	   of NONE => Error.error["unknown target \"", s, "\""]
	    | SOME(_, act) => TargetOpt act
	  (* end case *))

    fun targetHelp () = let
	  fun f [] = [")"]
	    | f [(s, _)] = [s, ")"]
	    | f ((s, _)::r) = s :: "," :: f r
	  in
	    String.concat("specify target (" :: f(!targets))
	  end

    fun mkOptions () = [
	    { short="h", long=["help"],
	      desc=(G.NoArg (fn () => HelpOpt)),
              help="this help message"
	    },
            { short="v", long=["verbose"],
              desc= G.OptArg(fn (SOME s) => VerboseOpt (getOpt (Int.fromString (s),0))
                              | (NONE) => VerboseOpt (1),"degree"),
              help="verbose mode (0,1,2)"
	    },
            { short="t", long=["target"],
	      desc= G.ReqArg(getTarget, "target"),
              help= targetHelp()
	    }
	  ]

    fun usage () = G.usageInfo {header=header, options=mkOptions ()}

    fun doOpts argv = G.getOpt {
	    argOrder = G.RequireOrder,
            options = mkOptions(),
            errFn = fn s => Error.error [s]
	  } argv

  end

