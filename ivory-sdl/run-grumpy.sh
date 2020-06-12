set -xe
cabal new-run ivory-sdl-grumpy
cd cgen && make && ./grumpy
