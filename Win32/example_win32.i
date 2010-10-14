read a;
read b;
write a + b;
write a;
while a > 0 and true do
    write a;
    a := a - 1;
done
c := b - 2;
if b = 6 then
    write 123;
else
    write 124;
fi
read c;
if c = 0 then
    abort;
else
    write 888;
fi
