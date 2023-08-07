clean:
	@rm -f \
		asm/p5_printf_f64.o asm/p5_main.o asm/p5 \
		asm/p5.o asm/p5 \
		asm/p7_f.o asm/p7_printf_f64.o asm/p7_main.o asm/p7 \
		asm/p7.o asm/p7

p5:
	@nasm -g -felf64 asm/p5_printf_f64.s -o asm/p5_printf_f64.o
	@nasm -g -felf64 asm/p5_main.s -o asm/p5_main.o
	@gcc -z noexecstack -o asm/p5 \
		asm/p5_printf_f64.o \
		asm/p5_main.o
	@./asm/p5

p5m:
	@nasm -g -felf64 asm/p5.s -o asm/p5.o
	@gcc -z noexecstack -o asm/p5 asm/p5.o
	@./asm/p5

p7:
	@nasm -g -felf64 asm/p7_f.s -o asm/p7_f.o
	@nasm -g -felf64 asm/p7_printf_f64.s -o asm/p7_printf_f64.o
	@nasm -g -felf64 asm/p7_main.s -o asm/p7_main.o
	@gcc -z noexecstack -o asm/p7 \
		asm/p7_f.o \
		asm/p7_printf_f64.o \
		asm/p7_main.o
	@./asm/p7

p7m:
	@nasm -g -felf64 asm/p7.s -o asm/p7.o
	@gcc -z noexecstack -o asm/p7 asm/p7.o
	@./asm/p7
