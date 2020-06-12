MODULES=main_game.c
CFLAGS=-Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow $(shell pkg-config --cflags sdl2)
LDFLAGS=$(shell pkg-config --libs sdl2)

all: main.c
	gcc -g ${CFLAGS} -o grumpy main.c ${MODULES} ${LDFLAGS}

opt: main.c
	gcc -O3 ${CFLAGS} -o grumpy main.c ${MODULES} ${LDFLAGS}

small: main.c
	gcc -Os ${CFLAGS} -o grumpy main.c ${MODULES} ${LDFLAGS}
	-strip grumpy
	-sstrip grumpy

clean:
	rm grumpy
