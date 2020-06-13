let
  project = import ./project {
    nixpkgs = import ./pkgs.nix;
  };
in project {
  packages = {
    simple-rts = ./simple-rts;
    ivory-memory = ./ivory-memory;
    ivory-nuklear = ./ivory-nuklear;
    ivory-sdl = ./ivory-sdl;
    ivory-string = ./ivory-string;
  };
}
