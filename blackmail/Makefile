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

test: add1-40.run add1-add1-40.run sub1-40.run
	@test "$(shell ./add1-40.run)" = "41"
	@test "$(shell ./add1-add1-40.run)" = "42"
	@test "$(shell ./sub1-40.run)" = "39"
