UNAME := $(shell uname)
.PHONY: test

%.run: %.o main.o
	gcc main.o $< -o $@

main.o: main.c
	gcc -c main.c -o main.o

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

test: 42.run
	@test "$(shell ./42.run)" = "42"
