MODULES=main_game.c
CFLAGS=-Wall -pedantic -Werror -Wshadow -Wstrict-aliasing -Wstrict-overflow $(shell pkg-config --cflags sdl2 gl glu glew)
LDFLAGS=$(shell pkg-config --libs sdl2 gl glu glew) -lm

all: main.c
	gcc -g ${CFLAGS} -o ivory-nuklear main.c ${MODULES} ${LDFLAGS}

opt: main.c
	gcc -O3 ${CFLAGS} -o ivory-nuklear main.c ${MODULES} ${LDFLAGS}

small: main.c
	gcc -Os ${CFLAGS} -o ivory-nuklear main.c ${MODULES} ${LDFLAGS}
	-strip ivory-nuklear
	-sstrip ivory-nuklear

clean:
	rm ivory-nuklear
