# 838eLLVM
838e languages with LLVM IR

```
                ┌──────┐
                │racket│
                └───┬──┘
   compile-file.rkt │
                    ▼
             ┌────────────┐
             │LLVM IR(.ll)│
             └──────┬─────┘
            llvm-as │
                    ▼
          ┌───────────────────┐
          │LLVM Byte Code(.bc)│
          └─────────┬─────────┘
                llc │
                    ▼
           ┌────────────────┐
           │x86 assembly(.s)│
           └────────┬───────┘
                gcc │
                    ▼
           ┌────────────────┐
           │machine code(.o)│
           └────────┬───────┘
ld link with main.o │
                    ▼
           ┌────────────────┐
           │executable(.run)│
           └────────────────┘
```
