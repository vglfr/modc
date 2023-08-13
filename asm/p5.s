global main

extern printf

section .data
        ?F:         db "%.2f", 10, 0
        ?0:         dq 5.0
        ?1:         dq 2.0

section .bss
        ?R:         resq 1
        x:          resq 1

section .text
?printf_f64:
        push        rbp
        mov         rbp, rsp

        mov         rdi, ?F
        mov         rax, 1
        movsd       xmm0, qword [rbp+16]
        call        printf

        pop         rbp
        xor         rax, rax
        ret

main:
        ; x = 5 - 2
        ; main = x * 2

        ; [x] <- [?0] - [?1]
        fld         qword [?0]
        fsub        qword [?1]
        fstp        qword [x]

        ; [?R] <- [x] * [?1]
        fld         qword [x]
        fmul        qword [?1]
        fstp        qword [?R]

        ; ?printf_f64 [?R]
        push        qword [?R]
        call        ?printf_f64
        add         rsp, 8

        ret
