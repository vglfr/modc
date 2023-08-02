global _f

section .data
        C0          dq 2.0

section .bss
        it          resq 1

section .text
_f:
        ; f x = 2 - x

        push        rbp
        mov         rbp, rsp

        ; rax <- [C0] - [a1]
        fld         qword [C0]
        fsub        qword [rbp+16]
        fstp        qword [it]
        mov         rax, [it]

        pop         rbp
        ret
