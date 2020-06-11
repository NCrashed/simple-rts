#include <SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>

int main(int argc, char *argv[])
{
  char bpath[PATH_MAX];
  if (realpath(argv[0], bpath) != NULL) {
      printf("Current binary path: %s\n", bpath);
  } else {
      perror("realpath() error");
      return 1;
  }

	if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
		fprintf(stderr, "SDL_Init Error: %s\n", SDL_GetError());
		return EXIT_FAILURE;
	}

	SDL_Window *win = SDL_CreateWindow("Hello World!", 100, 100, 620, 387, SDL_WINDOW_SHOWN);
	if (win == NULL) {
		fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
		return EXIT_FAILURE;
	}

	SDL_Renderer *ren = SDL_CreateRenderer(win, -1,SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if (ren == NULL) {
		fprintf(stderr, "SDL_CreateRenderer Error: %s\n", SDL_GetError());
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
		return EXIT_FAILURE;
	}

  char grumpy_src[PATH_MAX];
  strcpy(grumpy_src, dirname(bpath));
  strcat(grumpy_src, "/../img/grumpy-cat.bmp");
	SDL_Surface *bmp = SDL_LoadBMP(grumpy_src);
	if (bmp == NULL) {
		fprintf(stderr, "SDL_LoadBMP Error: %s\n", SDL_GetError());
		if (ren != NULL) {
			SDL_DestroyRenderer(ren);
		}
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
		return EXIT_FAILURE;
	}

	SDL_Texture *tex = SDL_CreateTextureFromSurface(ren, bmp);
	if (tex == NULL) {
		fprintf(stderr, "SDL_CreateTextureFromSurface Error: %s\n", SDL_GetError());
		if (bmp != NULL) {
			SDL_FreeSurface(bmp);
		}
		if (ren != NULL) {
			SDL_DestroyRenderer(ren);
		}
		if (win != NULL) {
			SDL_DestroyWindow(win);
		}
		SDL_Quit();
		return EXIT_FAILURE;
	}
	SDL_FreeSurface(bmp);

	for (int i=0; i < 20; i++) {
			SDL_RenderClear(ren);
			SDL_RenderCopy(ren, tex, NULL, NULL);
			SDL_RenderPresent(ren);
			SDL_Delay(100);
	}

	SDL_DestroyTexture(tex);
	SDL_DestroyRenderer(ren);
	SDL_DestroyWindow(win);
	SDL_Quit();

	return EXIT_SUCCESS;
}
