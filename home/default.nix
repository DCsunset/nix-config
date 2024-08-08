{ pkgs, dc-lib, ... }:

{
  imports = dc-lib.importSubdirs ./.;

  home.packages = with pkgs; [
    nodePackages.cspell
    xclip
    (python3.withPackages (ps: with ps; [
      matplotlib
    ]))
  ];
}
