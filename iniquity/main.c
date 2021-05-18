#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "runtime.h"

FILE* in;
FILE* out;
void (*error_handler)();

#define heap_size 10000
int64_t *heap;

void print_result(int64_t);
void print_char(int64_t);
void print_cons(int64_t);

void error_exit() {
  printf("err\n");
  exit(1);
}

void raise_error() {
  return error_handler();
}

int main(int argc, char** argv) {
  in = stdin;
  out = stdout;
  error_handler = &error_exit;

  heap = calloc(8*heap_size, 1);

  print_result(entry(heap));
  putchar('\n');

  free(heap);
  return 0;
}

void print_result(int64_t result) {
  if (cons_type_tag == (ptr_type_mask & result)) {
    printf("'(");
    print_cons(result);
    printf(")");
  } else if (box_type_tag == (ptr_type_mask & result)) {
    printf("#&");
    print_result (*((int64_t *)(result ^ box_type_tag)));
  } else if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64, result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } else {
    switch (result) {
    case val_true:
      printf("#t"); break;
    case val_false:
      printf("#f"); break;
    case val_eof:
      printf("#<eof>"); break;
    case val_empty:
      printf("'()"); break;
    case val_void:
      /* nothing */ break;
    }
  }
}

void print_cons(int64_t a) {
  int64_t car = *((int64_t *)((a + 8) ^ cons_type_tag));
  int64_t cdr = *((int64_t *)((a + 0) ^ cons_type_tag));
  print_result(car);
  if (cdr == val_empty) {
    // nothing
  } else if (cons_type_tag == (ptr_type_mask & cdr)) {
    printf(" ");
    print_cons(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}
