let
  pkgs = import <nixpkgs> { };
  myPkg = pkgs.haskellPackages.callPackage ./default.nix { };
in
  myPkg.env