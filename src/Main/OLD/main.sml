(* main.sml
 *
 * Main driver file for ml-idl.
 *)


structure MLIdl = struct
  
  structure G = GetOpt

  structure GenerateSMLRuntime = GenerateSMLFn (structure C = ConcreteRuntime) 

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  datatype target = MobyTarget
                  | ClassicTarget
                  | ClassicSmlTarget
                  | ClassicCTarget

  datatype optFlag = HelpOpt
                   | VerboseOpt of int
                   | TargetOpt of string

  val target : target option ref = ref (NONE)

  fun translateFile (file) = IdlTranslate.translate (Util.withInputFile (file,IdlParser.parse),file)
    handle IdlExn.Lexer e => Error.error [e]
         | IdlExn.Parser e => Error.error [e]

  (*
   * process the idl file:
   * parse, and convert parse tree
   * and generate files
   *
   *)              
  fun processFile (file) = let 
    val _ = Verbose.message1 ["Input file ",file]
    val ir = translateFile (file)
    fun doClassicSml () = let
      val files = GenerateSMLRuntime.generate (file,ir)
      fun f os = (Util.out os ["Group is\n"];
                  app (fn x => Util.out os [Util.extract_file (x)]) files)
    in
      Util.withOutputFile (Util.FILE_SML,Util.replace_extension (file,"","cm"),f)
    end
    fun doClassicC () = GenerateRuntime.generate (file,ir)
    fun doMoby () = GenerateMBX.generate (file,ir)
  in
    case (!target)
      of NONE => Error.error ["No target specified"]
       | SOME (ClassicSmlTarget) => (doClassicSml (); 
                                     OS.Process.success)
       | SOME (ClassicCTarget) => (doClassicC ();
                                   OS.Process.success)
       | SOME (ClassicTarget) => (doClassicSml ();
                                  doClassicC ();
                                  OS.Process.success)
       | SOME (MobyTarget) => (doMoby ();
                               OS.Process.success)
(*       | _ => fail ["MLIdl.processFile","Unimplemented target"] *)
  end


  (*
   * main entry point
   * parse commandline arguments, and process the required files
   *)
  fun main (name,cmdline) = let 
    val header = "Usage: ml-idl [OPTION...] file" 
    val options = [{short="h", long=["help"], desc=(G.NoArg (fn () => HelpOpt)),
                    help="this help message"},
                   {short="v", long=["verbose"],
                    desc=G.OptArg (fn (SOME s) => VerboseOpt (getOpt (Int.fromString (s),0))
                                    | (NONE) => VerboseOpt (1),"degree"),
                    help="Verbose mode (0,1,2)"},
                   {short="t", long=["target"],desc=G.ReqArg (fn (s) => TargetOpt (s),"target"),
                    help="Target"}]
    fun printUsage (s) = print (concat [s,G.usageInfo {header=header, options=options}])
    fun handleOpt (HelpOpt) = (printUsage ""; 
                               print "\n";
                               Error.quit ())
      | handleOpt (VerboseOpt (i)) = (Verbose.set (i))
      | handleOpt (TargetOpt (s)) = (target := (case s
                                                  of "moby" => SOME (MobyTarget)
                                                   | "classic" => SOME (ClassicTarget)
                                                   | "classic-sml" => SOME (ClassicSmlTarget)
                                                   | "classic-c" => SOME (ClassicCTarget)
                                                   | _ => Error.error ["Unrecognized target"]))
    fun main' (name,cmdline) = let
      (* process command line option *)
      val (opts,args) = G.getOpt {argOrder = G.RequireOrder,
                                  options = options,
                                  errFn = fn s => Error.error [s]} cmdline
    in
      app handleOpt opts;
      case (args)
        of [] => Error.error ["Need to specify an IDL file"]
         | [file] => processFile (file)
         | _ => Error.error ["Cannot handle multiple IDL files"]
    end
  in
    (* initialization *)
    Verbose.set (0);
    target := NONE;
    (* commandline arguments and actual processing *)
    main' (name,cmdline) handle e => (Error.except (e); OS.Process.failure)
  end

end



structure Main = struct

  fun main (prog,cmdline) = MLIdl.main (prog,cmdline)

  fun shell cmdline =  let 
    val (hd::tl) = String.tokens (fn #" " => true | _ => false) cmdline
  in
    main (hd,tl)
  end

end
