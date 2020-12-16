#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value sum()
{
    return Val_int(5);
}