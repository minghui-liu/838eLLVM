#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdint.h>
#include <stdio.h>

int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

#endif
