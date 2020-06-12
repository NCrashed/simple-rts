#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <libgen.h>

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
