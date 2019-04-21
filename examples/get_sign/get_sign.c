/*
 * First KLEE tutorial: testing a small function
 */

#include <klee/klee.h>

int main() {
   int x;
   klee_make_symbolic(&x, sizeof(x), "x");
   // klee_assume(x == 1);
   if (x == 0) {
     klee_print_expr("x", x);
     klee_print_expr("RETURN", x);
     return x;
  }
  if (x < 0) {
    klee_print_expr("x", x);
    klee_print_expr("RETURN", -x);
    return -x;
  }
  else {
    klee_print_expr("x", x);
    klee_print_expr("RETURN", 1);
    return 1;
  }
} 
