{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, z3 }:
      mkDerivation {
        pname = "number-cross2";
        version = "0.1.0.0";
        sha256 = "0";
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ base z3 ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
