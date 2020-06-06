{
  # Nixpkgs to use (pinned version)
  nixpkgs ? (import <nixpkgs>)
}:
{
  /*
  Set of local project packages by package name and their relative location. Example:

  ```
  {
    package1-name = ./package1-src;
    package2-name = ./package2-src;
  }
  ```
  */
  packages ? { } # :: { <package name> :: Path }
}:
let
  pkgs = nixpkgs { inherit config; };
  lib  = pkgs.haskell.lib;
  config = {
    packageOverrides = new: rec {
      haskellPackages = new.haskellPackages.override { overrides = haskOverrides; };
      simple-rts-c = new.callPackage ../simple-rts.nix { simple-rts = pkgs.haskellPackages.simple-rts; };
    };
    allowBroken = true;
  };
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner  = "siers";
    repo   = "nix-gitignore";
    rev    = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ignore = gitignore.gitignoreSourceAux ''
    .stack-work
    dist
    dist-newstyle
    .ghc.environment*
    '';
  haskOverrides = new: old: projectPkgs new // overridesDir new old;
  projectPkgs = new: builtins.mapAttrs (name: src: new.callCabal2nix name (ignore src) {}) packages;
  overridesDir = new: old: lib.packagesFromDirectory { directory = ../derivations; } new old;
in {
  inherit pkgs;
  packages = builtins.mapAttrs (name: _: pkgs.haskellPackages."${name}") packages;
}
