


CC = gcc

CFLAGS = -O3 -I

LIBS = -lX11 -lm 

LIBPATH = -L/opt/X11/lib/


all:	xbs.c
	$(CC) $(CFLAGS) -o xbs xbs.c $(LIBPATH) $(LIBS)


clean:
	-rm *.o xbs
