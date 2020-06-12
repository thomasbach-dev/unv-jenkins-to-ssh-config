{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  drv = import ./default.nix { inherit nixpkgs compiler; };
in
  if nixpkgs.pkgs.lib.inNixShell then drv.env else drv
