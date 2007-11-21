(* ml-idl.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end  = struct

    fun translateFile (file) = (
	  Verbose.message1 ["Input file ",file];
	  IdlTranslate.translate (Util.withInputFile (file, IdlParser.parse), file)
	    handle IdlExn.Lexer e => Error.error [e]
        	 | IdlExn.Parser e => Error.error [e])

    fun main (cmd, args) = let
	  val (options, files) = CmdOptions.doOpts args
	  fun doOpt CmdOptions.HelpOpt = (
		print(CmdOptions.usage());
		print "\n";
        	Error.quit ())
	    | doOpt (CmdOptions.VerboseOpt i) = (Verbose.set i; NONE)
	    | doOpt (CmdOptions.TargetOpt act) = SOME act
	  in
	    case (List.mapPartial doOpt options, files)
	     of (_, []) => Error.error ["Need to specify an IDL file"]
	      | ([], _) => Error.error ["Need to specify a target"]
	      | (targets, files) => let
		  fun doFile f = let
			val spec = translateFile f
		        val {dir, file} = OS.Path.splitDirFile f
			val {base, ...} = OS.Path.splitBaseExt file
			fun doTarget act = act {
				srcDir = dir,
				srcFile = base,
				dstDir = NONE,
				spec = spec
			      }
			in
			  List.app doTarget targets
			end
		  in
		    List.app doFile files; OS.Process.success
		  end
	    (* end case *)
	  end
	    handle Error.Quit => OS.Process.failure
		 | ex => (
		     TextIO.output(TextIO.stdErr, concat[
			 "uncaught exception ", exnName ex,
			 " [", exnMessage ex, "]\n"
		       ]);
		     app (fn s => TextIO.output(TextIO.stdErr, concat[
			 "  raised at ", s, "\n"
		       ]))
		       (SMLofNJ.exnHistory ex);
		     OS.Process.failure)

  end
