#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>

#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_STANDARD_VARARGS
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#define NK_IMPLEMENTATION
#define NK_SDL_GL3_IMPLEMENTATION
#include <GL/glew.h>
#include <SDL_opengl.h>
#include "main_game.h"

int main(int argc, char *argv[])
{
  char bpath[PATH_MAX];
  if (realpath(argv[0], bpath) != NULL) {
      printf("Current binary path: %s\n", bpath);
  } else {
      perror("realpath() error");
      return 1;
  }

	return run_game(dirname(bpath));
}
