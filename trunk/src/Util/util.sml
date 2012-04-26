(* util.sml
 *
 * Utility functions
 *)

structure Util =
  struct

  datatype file_type = FILE_SML | FILE_C | FILE_CXX | FILE_MAKEFILE | FILE_MBX
  
  local 
    fun curry f x = (fn y => f (x,y))
    fun appC f x y = app (curry f x) y
  in
    fun out os l = (appC TextIO.output os l; 
                    TextIO.output (os,"\n"))
    fun outF os str lst = out os [Format.format str lst]
  end

  local
    val i = ref (0)
  in
    fun gensym () = let
	  val i' = !i
	  in
	    i := (i' + 1);
	    "tmp_"^ Int.toString i'
	  end
  end

  (* convert a file name that might have "-" and "." characters to a CPP symbol.  For
   * example, "foo-bar.hxx" is mapped to "__FOO_BAR_HXX__".
   *)
    fun toCPPSymbol s = let
	  fun tr #"." = #"_"
	    | tr #"-" = #"_"
	    | tr c = Char.toUpper c
	  in
	    String.concat["__", String.map tr s, "__"]
	  end

  fun commentStrings (FILE_SML) = {start="(*",finish=" *)", cont=" *"}
    | commentStrings (FILE_C) = {start="/*",finish=" */",cont=" *"}
    | commentStrings (FILE_CXX) = {start="//",finish="//",cont="//"}
    | commentStrings (FILE_MAKEFILE) = {start="#",finish="#",cont="#"}
    | commentStrings (FILE_MBX) = {start="//", finish="//",cont="//"}

  fun comment os (ft,lines) = let
    val {start,finish,cont} = commentStrings (ft)
  in
    out os [];
    out os [start];
    app (out os) (map (fn (s) => [cont^" "^s]) lines);
    out os [finish];
    out os []
  end
    
  fun concatSep (s, l) = String.concatWith s l

  fun stripString (s) = let
    val ss = Substring.full (s)
  in
    Substring.string (Substring.triml 1 (Substring.trimr 1 ss))
  end
    
  fun mapI f l = let
    fun iterMapI f [] i = []
      | iterMapI f (x::xs) i = (f (x,i))::iterMapI f xs (i+1)
  in
    iterMapI f l 0
  end

  fun appI f l = let
    fun iterAppI f [] i = ()
      | iterAppI f (x::xs) i = (f (x,i); iterAppI f xs (i+1))
  in
    iterAppI f l 0
  end

  fun dateString () = Date.toString(Date.fromTimeLocal(Time.now()))

  fun generated_message os ft = let
    val d = Date.fromTimeLocal (Time.now ())
    in
      comment os (ft,[
	  "This file was automatically generated by ml-idl",
	  String.concat["(", dateString(), ")"]
	])
    end


  fun withOutputFile (ft,file, f) = let
    val os = TextIO.openOut (file)
    val _ = generated_message os ft
    val r = f (os)
  in
    TextIO.closeOut (os);
    r
  end


  fun withInputFile (file, f) = let
    val is = TextIO.openIn (file)
    val r = f (is)
  in
    TextIO.closeIn (is);
    r
  end

  fun replace_extension (f,a,e) = let
    val {base,...} = OS.Path.splitBaseExt (f)
  in
    OS.Path.joinBaseExt {base=base^a,ext=SOME(e)}
  end

  (* take a path+file f, and replace the filename by f' *)
  fun replace_file (f,f') = let
    val {dir,file} = OS.Path.splitDirFile (f)
  in
    OS.Path.joinDirFile {dir=dir,file=f'}
  end

  fun extract_file (f) = let
    val {file,...} = OS.Path.splitDirFile (f)
  in
    file
  end

end
