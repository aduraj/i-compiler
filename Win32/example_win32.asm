segment .data

print_text db "%d", 10, 13, 0
scan_text db "%d", 0
var_a dd 0
var_b dd 0
var_c dd 0

segment .text

extern _printf, _scanf

global _main
_main:

push var_a
push dword scan_text
call _scanf
add esp, 8

push var_b
push dword scan_text
call _scanf
add esp, 8

mov ebx, dword [var_a]
push ebx
mov ebx, dword [var_b]
push ebx
pop ebx
pop ecx
add ebx, ecx
push ebx
push print_text
call _printf
add esp, 8

push dword [var_a]
push print_text
call _printf
add esp, 8

ICTOWHILESTART0:
mov ebx, dword [var_a]
push ebx
mov ebx, 0
push ebx
pop ecx
pop ebx
cmp ebx, ecx
jg ICRELATION0
mov ebx, 0
jmp ICRELATION1
ICRELATION0:
mov ebx, 1
ICRELATION1:
push ebx
mov ebx, 1
push ebx
pop ecx
pop ebx
and ebx, ecx
mov ecx, 1
cmp ebx, ecx
je ICTOWHILEBODY0
jmp ICTODONE1
ICTOWHILEBODY0:
push dword [var_a]
push print_text
call _printf
add esp, 8

mov ebx, dword [var_a]
push ebx
mov ebx, 1
push ebx
pop ecx
pop ebx
sub ebx, ecx
mov dword [var_a], ebx
jmp ICTOWHILESTART0
ICTODONE1:
mov ebx, dword [var_b]
push ebx
mov ebx, 2
push ebx
pop ecx
pop ebx
sub ebx, ecx
mov dword [var_c], ebx
mov ebx, dword [var_b]
push ebx
mov ebx, 6
push ebx
pop ecx
pop ebx
cmp ebx, ecx
je ICRELATION4
mov ebx, 0
jmp ICRELATION5
ICRELATION4:
mov ebx, 1
ICRELATION5:
mov ecx, 1
cmp ebx, ecx
je ICTOTHEN4
jmp ICTOELSE4
ICTOTHEN4:
push 123
push print_text
call _printf
add esp, 8

jmp ICTOFI5
ICTOELSE4:
push 124
push print_text
call _printf
add esp, 8

ICTOFI5:
push var_c
push dword scan_text
call _scanf
add esp, 8

mov ebx, dword [var_c]
push ebx
mov ebx, 0
push ebx
pop ecx
pop ebx
cmp ebx, ecx
je ICRELATION8
mov ebx, 0
jmp ICRELATION9
ICRELATION8:
mov ebx, 1
ICRELATION9:
mov ecx, 1
cmp ebx, ecx
je ICTOTHEN8
jmp ICTOELSE8
ICTOTHEN8:
jmp exit
jmp ICTOFI9
ICTOELSE8:
push 888
push print_text
call _printf
add esp, 8

ICTOFI9:
exit:
ret
