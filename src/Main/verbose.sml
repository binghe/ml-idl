(* verbose.sml
 *
 *)

structure Verbose = struct
  
  val flag = ref (0)
    
  fun set (i) = (flag := i)
    
  fun verbose (i,f) = 
    if (!flag >= i) then f () else ()
      
  fun message1 l = verbose (1,fn () => (app print l; print "\n"))
  fun message2 l = verbose (2,fn () => (app print l; print "\n"))
    
end

