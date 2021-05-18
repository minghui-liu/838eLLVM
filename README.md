# 838eLLVM

838e languages with LLVM IR

```
                     +------+
                     |racket|
                     +---+--+
                         |
        compile-file.rkt |
                         v
                  +------------+
                  |LLVM IR(.ll)|
                  +------+-----+
                     llc |
             +-----------+-----------+
             |                       |
             v                       v
     +--------------+        +----------------+
     |x86 object(.o)|        |wasm object(.wo)|
     +-------+------+        +-------+--------+
ld runtime.o |                       | ld runtime.wo
             v                       v
    +----------------+       +------------------+
    |executable(.run)|       |wasm module(.wasm)|
    +----------------+       +------------------+
```

# Build

Besides racket, [Emscripten](https://emscripten.org/index.html),
gcc, and llvm are required to build the executables.

For x86 executables

```console
$ cat > 42.rkt <<EOF
#lang racket
42
$ make 42.run
gcc -fPIC -c -o main.o main.c
gcc -fPIC -c -o char.o char.c
gcc -fPIC -c -o io.o io.c
ld -r main.o char.o io.o -o runtime.o
racket -t compile-file.rkt -m 42.rkt > 42.ll
llc -march=x86-64 -filetype=obj -o 42.o 42.ll
gcc runtime.o 42.o -o 42.run
rm 42.o 42.ll
$ ./42.run
42
```

For wasm,

```console
$ make 42.wasm
emcc -c -o main.wo main.c
emcc -c -o char.wo char.c
emcc -c -o io.wo io.c
emcc -r main.wo char.wo io.wo -o runtime.wo
racket -t compile-file.rkt -m 42.rkt > 42.ll
llc -march=wasm32 -filetype=obj -o 42.wo 42.ll
emcc -o 42.wasm 42.wo runtime.wo
rm 42.wo 42.ll
$ wasmer 42.wasm
42
```

[Wasmer](https://wasmer.io/) is used to run wasm
executables. Browser support pending.
