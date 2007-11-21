(* error.sml
 *
 *)

structure Error : sig

  exception Quit

  val quit : unit -> 'a
  val error : string list -> 'a
  val bug : string list -> unit
  val warning : string list -> unit
  val except : exn -> unit

  (* attach a function source to an error report *)
  val funct : string * string list -> string list

end = struct

  exception Quit

  fun report (s,l) = (app print (s::l); print "\n")

  fun quit () = raise Quit

  fun error (l) = (report ("ERROR --- ",l); raise Quit)

  fun bug (l) = report ("BUG --- ",l)
                   
  fun warning l = report ("WARNING --- ",l)
                   
  fun except (Quit) = ()
    | except (e) = report ("EXCEPTION --- ",[exnMessage e])

  fun funct (f,l) = l@[concat [" [function ",f,"]"]]
                   
end
