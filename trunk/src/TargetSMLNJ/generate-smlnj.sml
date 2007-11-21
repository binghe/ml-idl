(* generate-smlnj.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

structure GenerateSMLNJ : sig end =
  struct

    structure GenerateSMLRuntime = GenerateSMLFn (structure C = ConcreteRuntime) 

    fun doSMLNJ {srcDir, srcFile, dstDir, spec} = let
	  val files = GenerateSMLRuntime.generate (srcFile, spec)
	  fun f os = (
		Util.out os ["Group is\n"];
                List.app (fn x => Util.out os [Util.extract_file (x)]) files)
	  in
	    Util.withOutputFile(
	      Util.FILE_SML,
	      Util.replace_extension (srcFile, "", "cm"), f)
	  end

    val _ = CmdOptions.register ("smlnj", doSMLNJ)

  end
