objs = \
	main.o \
	char.o \
	io.o

default: runtime.o

main.o: types.h runtime.h
char.o: types.h
io.o: runtime.h

%.o: %.c
	gcc -fPIC -c -o $@ $<

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

# x86 backend: .ll -> .o -> .run

%.ll: %.rkt $(wildcard *.rkt)
	racket -t compile-file.rkt -m $< > $@

%.o: %.ll
	llc -march=x86-64 -filetype=obj -o $@ $<

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

# wasm backend: .ll -> .wo -> .wasm

%.wo: %.ll
	llc -march=wasm32 -filetype=obj -o $@ $<

%.wasm: %.wo
	wasm-ld --no-entry --export-all -o $@ $<

clean:
	rm -f *.ll *.o *.run *.wo *.wasm

.PHONY: clean
