## If this looks weird see `README.md`.

{ pkgs ? import <nixpkgs> {} }:
(pkgs.buildFHSUserEnv {
  name = "pipzone";
  targetPkgs = pkgs: (with pkgs; [
    glibc
    python310

    # python310Packages.mypy
    # This is supposedly mypy version 0.961,
    # but when I use it this way I get 0.91x.
    # When I install mypy via virtualenv I get the same thing,
    # but at least I'm able to then upgrade it.
    # See this project's README for more details.

    python310Packages.numpy
    python310Packages.pandas
    python310Packages.pip
    python310Packages.setuptools
    python310Packages.wheel
    virtualenv
    zlib
    zlib.dev
  ]);
  runScript = "bash";
}).env
