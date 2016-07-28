all: dinero.x86f

dinero.x86f: dinero.lisp #curses.x86f
	sbcl --noinform --noprint --eval "(compile-file \"dinero.lisp\")" --eval "(quit)"

#curses.x86f: curses.lisp
#	sbcl --noinform --noprint --eval "(compile-file \"curses.lisp\")" --eval "(quit)"
