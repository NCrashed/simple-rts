let
  project = import ./project {
    nixpkgs = import ./pkgs.nix;
  };
in project {
  packages = {
    simple-rts = ./simple-rts;
  };
}
