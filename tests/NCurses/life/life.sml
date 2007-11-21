
structure Life = struct

  structure R = Random
  structure A = Array2
  structure N = NCurses

  local 
    val e = R.rand (Int32.toInt (Time.toSeconds (Time.now ())),0)
  in
    fun randRange (n) = R.randRange (0,n-1) e
  end

  datatype cell = On | Off

  val empty = A.array (0,0,Off)

  val grid = ref (empty)
  val back = ref (empty)

  fun dump_grid () = let
    val i = Int32.fromInt
    fun doit (x,y,cell) = let
      val str = case cell of On => "@" | Off => " "
    in
      N.move (i y,i x);
      N.addstr (str)
    end
  in
    A.appi A.RowMajor (ignore o doit) {base= !grid,col=0,row=0,nrows=NONE,ncols=NONE};
    N.refresh ()
  end

  fun neighbors (x,y) = let
    fun isOn (x,y) = case (A.sub (!grid,x,y)) of On => 1 | Off => 0
  in
    foldl (fn ((x,y),c) => isOn (x,y)+c) 0 [(x-1,y-1),(x-1,y),(x-1,y+1),
                                            (x,y-1),(x,y+1),
                                            (x+1,y-1),(x+1,y),(x+1,y+1)]
  end

  fun loop () = loop ()
  fun delay (s) = TimeLimit.timeLimit (Time.fromMilliseconds (s*1000)) loop () handle _ => ()
        
  fun generation (0) = dump_grid ()
    | generation (n) = let
        fun doit (x,y,On) = let
                val n = neighbors (x,y)
              in
                A.update (!back,x,y, if n=2 orelse n=3 then On else Off)
              end
          | doit (x,y,Off) = let
                val n = neighbors (x,y)
              in
                A.update (!back,x,y, if n=3 then On else Off)
              end
        val temp = !grid
        val rows = A.nRows (temp)
        val cols = A.nCols (temp)
      in
        A.appi A.RowMajor doit {base=(!grid),row=1,col=1,nrows=SOME (rows-2), ncols=SOME (cols-2)};
        grid := (!back);
        back := temp;
        dump_grid ();
        delay (1);
        generation (n-1)
      end


  fun life (x,y,lp,gen) = let
    val g = A.array (x,y,Off)
  in
    app (fn (x,y) => A.update (g,x,y,On)) lp;
    grid := g;
    back := A.array (x,y,Off);
    N.initscr ();
    N.clear ();
    generation (gen);
    N.endwin ()
  end

  fun random (x,y,n,gen) = let
    val l = List.tabulate (n,fn _ => ((randRange (x-2))+1,(randRange (y-2))+1))
  in
    life (x,y,l,gen)
  end
    

end
