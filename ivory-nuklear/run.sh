set -xe
cabal new-run ivory-nuklear-demo
cd cgen && make && ./ivory-nuklear
