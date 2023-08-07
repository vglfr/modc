global main

extern printf

section .data
        FST:        db "%.2f", 10, 0
        C0          dq 5.0
        C1          dq 2.0

section .bss
        it          resq 1
        x           resq 1

section .text
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
        ; x = 5 - 2
        ; main = x * 2

        ; [x] <- [C0] - [C1]
        fld         qword [C0]
        fsub        qword [C1]
        fstp        qword [x]

        ; [it] <- [x] * [C1]
        fld         qword [x]
        fmul        qword [C1]
        fstp        qword [it]

        ; printf_f64 [it]
        push        qword [it]
        call        printf_f64
        add         rsp, 8

        ret
