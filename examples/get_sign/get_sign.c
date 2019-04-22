/*
 * First KLEE tutorial: testing a small function
 */

#include <klee/klee.h>

int main() {
   int x;
   klee_make_symbolic(&x, sizeof(x), "x");
   // klee_assume(x == 1);
   if (x == 0) {
     x = x + 1;
   } else if (x < 0) {
     x = -x;
   } else {
     x = 1;
   }
   return x;
} 
