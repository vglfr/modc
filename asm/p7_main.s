global main

extern _printf_f64
extern _fadd

section .data
        C1:         dq 1.1111111
        C2:         dq 2.2222222

section .bss
        RES         resq 1

section .text
main:
        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        push        qword [C2]
        call        _fadd
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        push        qword [C2]
        push        qword [C2]
        call        _fadd
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        ret
