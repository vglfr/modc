global main

extern _f
extern _printf_f64

section .data
        C0          dq 3.0
        C1          dq 2.0
        C2          dq 1.0

section .bss
        it          resq 1

section .text
main:
        ; main = f 3 - 2 + f 1

        ; sm7 <- f [C0]
        push        qword [C0]
        call        _f
        add         rsp, 8
        mov         qword [it], rax
        fld         qword [it]

        ; sm7 <- sm7 - [C1]
        fsub        qword [C1]

        ; sm7 <- f [C2]
        push        qword [C2]
        call        _f
        add         rsp, 8
        mov         qword [it], rax
        fld         qword [it]

        ; [it] <- sm6 + sm7
        fadd
        fstp        qword [it]

        ; printf_f64 [it]
        push        qword [it]
        call        _printf_f64
        add         rsp, 8

        ret
