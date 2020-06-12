let
  project = import ./project {
    nixpkgs = import ./pkgs.nix;
  };
in project {
  packages = {
    simple-rts = ./simple-rts;
    ivory-sdl = ./ivory-sdl;
    ivory-memory = ./ivory-memory;
    ivory-string = ./ivory-string;
  };
}
