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
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override { overrides = haskOverrides; };
    };
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
  haskOverrides = new: old: builtins.mapAttrs (name: src: new.callCabal2nix name (ignore src) {}) packages;
in {
  inherit pkgs;
  packages = builtins.mapAttrs (name: _: pkgs.haskellPackages."${name}") packages;
}
