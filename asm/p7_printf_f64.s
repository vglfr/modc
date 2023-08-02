global _printf_f64

extern printf

section .data
        FST:        db "%.2f", 10, 0

section .text
_printf_f64:
        push        rbp
        mov         rbp, rsp

        mov         rdi, FST
        mov         rax, 1
        movsd       xmm0, qword [rbp+16]
        call        printf

        pop         rbp
        xor         rax, rax
        ret
