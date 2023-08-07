global main

extern printf

section .data
        FST:        db "%.2f", 10, 0
        C0          dq 3.0
        C1          dq 2.0
        C2          dq 1.0

section .bss
        it          resq 1

section .text
f:
        ; f x = 2 - x

        push        rbp
        mov         rbp, rsp

        ; rax <- [C0] - [a1]
        fld         qword [C1]
        fsub        qword [rbp+16]
        fstp        qword [it]
        mov         rax, [it]

        pop         rbp
        ret

printf_f64:
        push        rbp
        mov         rbp, rsp

        mov         rdi, FST
        mov         rax, 1
        movsd       xmm0, qword [rbp+16]
        call        printf

        pop         rbp
        xor         rax, rax
        ret

main:
        ; main = f 3 - 2 + f 1

        ; sm7 <- f [C0]
        push        qword [C0]
        call        f
        add         rsp, 8
        mov         qword [it], rax
        fld         qword [it]

        ; sm7 <- sm7 - [C1]
        fsub        qword [C1]

        ; sm7 <- f [C2]
        push        qword [C2]
        call        f
        add         rsp, 8
        mov         qword [it], rax
        fld         qword [it]

        ; [it] <- sm6 + sm7
        fadd
        fstp        qword [it]

        ; printf_f64 [it]
        push        qword [it]
        call        printf_f64
        add         rsp, 8

        ret
