objs = \
	main.o \
	char.o \
	io.o

objs_wasm = ${objs:.o=.wo}

default: runtime.o runtime.wo

main.o: types.h runtime.h
char.o: types.h
io.o: runtime.h

%.ll: %.rkt $(wildcard *.rkt)
	racket -t compile-file.rkt -m $< > $@

# x86 backend: .ll -> .o -> .run

%.o: %.c
	gcc -fPIC -c -o $@ $<

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.o: %.ll
	llc -march=x86-64 -filetype=obj -o $@ $<

%.run: %.o runtime.o
	gcc runtime.o $< -o $@

# wasm backend: .ll -> .wo -> .wasm

%.wo: %.c
	emcc -c -o $@ $<

runtime.wo: $(objs_wasm)
	emcc -r $(objs_wasm) -o runtime.wo

%.wo: %.ll
	llc -march=wasm32 -filetype=obj -o $@ $<

%.wasm: %.wo runtime.wo
	emcc -o $@ $< runtime.wo

# entry only
# %.wasm: %.wo
# 	wasm-ld --no-entry --export-all -o $@ $<

clean:
	rm -f *.ll *.o *.run *.wo *.wasm

.PHONY: clean
