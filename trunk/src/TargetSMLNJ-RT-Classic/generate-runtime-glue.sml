(* generate-runtime-glue.sml
 *
 *)


structure GenerateClassicRuntimeGlue : sig

    val generate : (string * IIL.iil) -> unit

  end = struct

  structure I = IIL
  structure A = Atom
  structure S = AtomMap
  structure F = Format
    
  open Util

  fun fail (x::xs) = (Error.bug (Error.funct (x,xs)); Error.quit ())

  fun dump_cfun_proto_list_h os = 
    (out os ["#ifndef _CFUN_PROTO_LIST_"];
     out os ["#define _CFUN_PROTO_LIST_"];
     out os ["#ifndef _C_LIBRARY_"];
     out os ["#  include \"c-library.h\""];
     out os ["#endif"];
     out os [];
     out os ["/* the external definitions for the C functions */"];
     out os ["#define CFUNC(NAME, FUNC, MLTYPE) CFUNC_PROTO(NAME, FUNC, MLTYPE)"];
     out os ["#include \"cfun-list.h\""];
     out os ["#undef CFUNC"];
     out os [];
     out os ["#endif /* !_CFUN_PROTO_LIST_ */"])

  fun dump_cfun_list_h (I.IIL {decls,clib_name=SOME(lib),clib_version,clib_date,...}) os = let
          fun outputOper (I.Operation (name,oper)) = 
                out os [F.format "CFUNC(\"%s\", ml_stub_%s, \"\")" [F.ATOM (name),F.ATOM (name)]]
            | outputOper _ = ()
        in
          out os ["#ifndef CLIB_NAME"];
          out os [F.format "#define CLIB_NAME \"%s\"" [F.STR (lib)]];
          out os [F.format "#define CLIB_VERSION \"%s\"" [F.STR (Option.getOpt (clib_version,""))]];
          out os [F.format "#define CLIB_DATE \"%s\"" [F.STR (Option.getOpt (clib_date,""))]];
          out os ["#endif"];
          out os [];
          app outputOper decls
        end
    | dump_cfun_list_h _ _ = Error.error ["IDL file missing required information (e.g. clib_name)"]

  fun dump_makefile file os = let
    (* for win32, thes will have to be "obj" instead and corr. for lib file... *)
    val obj_file = OS.Path.file (replace_extension (file,"","o"))
    val obj_clib_file = OS.Path.file (replace_extension (file,"-lib","o"))
    val lib_file = OS.Path.file (replace_extension (file,"","a"))
  in
    out os ["SHELL = /bin/sh"];
    out os ["INC_DIR = ../../include"];
    out os ["CLIB_DIR = ../"];
    out os ["INCLUDES = -I$(INC_DIR) -I$(CLIB_DIR) -I../../objs"];
    out os ["MAKE = make"];
    out os ["AR = ar"];
    out os ["ARFLAGS = rcv"];
    out os ["RANLIB = ranlib"];
    out os [F.format "LIBRARY = %s" [F.STR (lib_file)]];
    out os ["VERSION = v-dummy"];
    out os [F.format "OBJS = %s %s" [F.STR (obj_clib_file),F.STR (obj_file)]];
    out os [];
    out os ["$(LIBRARY) : $(VERSION) $(OBJS)"];
    out os ["\trm -rf $(LIBRARY)"];
    out os ["\t$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)"];
    out os ["\t$(RANLIB) $(LIBRARY)"];
    out os [];
    out os ["$(VERSION) :"];
    out os ["\t($(MAKE) MAKE=\"$(MAKE)\" clean)"];
    out os ["\techo \"$(VERSION)\" > $(VERSION)"];
    out os [];
    out os [".c.o: $(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \\"];
    out os ["$(CLIB_DIR)/ml-c.h cfun-proto-list.h cfun-list.h"];
    out os ["\t$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<"];
    out os [];
    out os ["clean :"];
    out os ["\trm -f v-* *.o $(LIBRARY)"]
  end


  fun dump_lib_c (I.IIL {clib_name=SOME (lib),...}) os = 
      (out os ["#include \"ml-base.h\""];
       out os ["#include \"c-library.h\""];
       out os ["#include \"cfun-proto-list.h\""];
       out os [];
       out os ["/* the table of C functions and ML names */"];
       out os ["#define CFUNC(NAME, FUNC, MLTYPE) CFUNC_BIND(NAME, FUNC, MLTYPE)"];
       out os ["PVT cfunc_binding_t CFunTable[] = {"];
       out os ["#include \"cfun-list.h\""];
       out os ["  CFUNC_NULL_BIND"];
       out os ["};"];
       out os ["#undef CFUNC"];
       out os [];
       out os ["/* the  library */"];
       out os [F.format "c_library_t %s = {" [F.STR (lib)]];
       out os [" CLIB_NAME,"];
       out os [" CLIB_VERSION,"];
       out os [" CLIB_DATE,"];
       out os [" NIL(clib_init_fn_t),"];
       out os [" CFunTable"];
       out os ["};"])
    | dump_lib_c _ _ = Error.error ["IDL file missing required information (e.g. clib_name)"]


  fun generate (file, iil as (I.IIL {symtable,decls,...})) = let
    val clib_file = replace_extension (file,"-lib","c")
  in
    Verbose.message1 ["Generating glue files"];
    withOutputFile (FILE_C,replace_file (file,"cfun-proto-list.h"),dump_cfun_proto_list_h);
    withOutputFile (FILE_C,replace_file (file,"cfun-list.h"),dump_cfun_list_h iil);
(*    withOutputFile (FILE_MAKEFILE,replace_file (file,"makefile"),dump_makefile (file)); *)
    withOutputFile (FILE_C,clib_file,dump_lib_c iil)
  end

end
