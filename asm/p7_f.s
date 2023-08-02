global _fadd

section .bss
        RES         resq 1

section .text
_fadd:
        push        rbp
        mov         rbp, rsp

        fld         qword [rbp+24]
        fld         qword [rbp+16]
        fadd

        fstp        qword [RES]
        mov         rax, [RES]

        pop         rbp
        ret
