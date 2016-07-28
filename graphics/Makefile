all: graphics

LIBS = -lX11 -lXpm -lstdc++ 	# stdc++ needed for sqrt() for some weird reason
				# took me forever to figure out, too
LIBDIRS = -L/usr/X11R6/lib

INCLUDEDIRS = -I/usr/X11R6/include

IMGS = balls/red-ball.xpm balls/blue-ball.xpm balls/purple-ball.xpm balls/yellow-ball.xpm \
       balls/green-ball.xpm balls/icon-face.xpm

graphics: main.c timer.c $(IMGS)
	gcc main.c timer.c -o graphics $(INCLUDEDIRS) $(LIBDIRS) $(LIBS)

.PHONY: debug
debug: graphics.debug
	gdb graphics.debug

graphics.debug: main.c timer.c $(IMGS)
	gcc -g main.c timer.c -o graphics.debug $(INCLUDEDIRS) $(LIBDIRS) $(LIBS)

.PHONY: clean
clean:
	rm -f graphics graphics.debug

