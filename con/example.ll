define i64 @entry() {
%retval362 = alloca i64, align 4
%g365 = add i64 0, 1
%result363 = icmp eq i64 0, %g365
br i1 %result363, label %if360, label %if361
if360:
%g368 = add i64 0, 43
%g366 = sub i64 %g368, 1
store i64 %g366, i64* %retval362, align 4
br label %end364
if361:
%retval371 = alloca i64, align 4
%g374 = add i64 0, 0
%result372 = icmp eq i64 0, %g374
br i1 %result372, label %if369, label %if370
if369:
%g375 = add i64 0, 1024
store i64 %g375, i64* %retval371, align 4
br label %end373
if370:
%g376 = add i64 0, 2048
store i64 %g376, i64* %retval371, align 4
br label %end373
end373:
%g367 = load i64, i64* %retval371
store i64 %g367, i64* %retval362, align 4
br label %end364
end364:
%g359 = load i64, i64* %retval362
ret i64 %g359
}

