let
  release = import ./release.nix;
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
    pkgs.SDL2
    pkgs.pkgconfig
    pkgs.elfkickers
  ];
  packages = _: pkgs.lib.attrValues release.packages;
}
