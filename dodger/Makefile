UNAME := $(shell uname)
.PHONY: test

%.run: %.o main.o char.o
	gcc main.o char.o $< -o $@

main.o: main.c types.h
	gcc -c main.c -o main.o

char.o: char.c types.h
	gcc -c char.c -o char.o

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

test: example.run example2.run
	@test "$(shell ./example.run)" = "1024"
	@test "$(shell ./example2.run)" = "42"
	@test "$(shell ./example3.run)" = "#\a"
