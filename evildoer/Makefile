UNAME := $(shell uname)
.PHONY: test

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

runtime.o: main.o char.o io.o
	ld -r main.o char.o io.o -o runtime.o

main.o: main.c types.h runtime.h
	gcc -fPIC -c main.c -o main.o

char.o: char.c types.h
	gcc -fPIC -c char.c -o char.o

io.o: io.c runtime.h
	gcc -fPIC -c io.c -o io.o

%.o: %.s
	gcc -c $< -o $@

%.s: %.bc
	llc $< -o $@

%.bc: %.ll
	llvm-as $< -o $@

%.ll: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm -f *.ll *.bc *.o *.s *.run
