all: cdrom.so cdp.x86f cdp-actions.x86f

cdrom.so: linux_cdrom.c
	gcc -c linux_cdrom.c
	ld -shared -o cdrom.so linux_cdrom.o

cdp.x86f: cdp.lisp
	sbcl --noinform --noprint --eval "(compile-file \"cdp.lisp\")" --eval "(quit)"

cdp-actions.x86f: cdp-actions.lisp
	sbcl --noinform --noprint --eval "(compile-file \"cdp-actions.lisp\")" --eval "(quit)"
