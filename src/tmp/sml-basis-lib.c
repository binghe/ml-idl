
/*
 * This file was automatically generated by ml-idl
 * (Thu Aug  9 16:47:54 2007)
 */

#include "ml-base.h"
#include "c-library.h"
#include "cfun-proto-list.h"

/* the table of C functions and ML names */
#define CFUNC(NAME, FUNC, MLTYPE) CFUNC_BIND(NAME, FUNC, MLTYPE)
PVT cfunc_binding_t CFunTable[] = {
#include "cfun-list.h"
  CFUNC_NULL_BIND
};
#undef CFUNC

/* the  library */
c_library_t SMLBasis = {
 CLIB_NAME,
 CLIB_VERSION,
 CLIB_DATE,
 NIL(clib_init_fn_t),
 CFunTable
};
