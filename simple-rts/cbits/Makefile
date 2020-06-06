MODULES=fib_test.c

all: main.c
	gcc -g -Wall -o simple-rts main.c ${MODULES}

opt: main.c
	gcc -Wall -O3 -o simple-rts main.c ${MODULES}

clean:
	rm simple-rts
