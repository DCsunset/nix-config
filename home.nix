{ pkgs, dc-lib, ... }:

{
  imports = dc-lib.importSubdirs ./home;

  home.packages = with pkgs; [
    nodePackages.cspell
    xclip
    (python3.withPackages (ps: with ps; [
      matplotlib
    ]))
  ];
}
