{ config, pkgs, lib, dc-lib, ... }:

let
  cfg = config.dc-home;
in
{
  imports = dc-lib.importSubdirs ./.;

  options.dc-home = {
    gui = {
      enable = lib.mkEnableOption "gui apps";
      displayServer = lib.mkOption {
        type = lib.types.enum [ "x11" "wayland" ];
        default = "x11";
        description = "Display server";
      };
    };
  };

  config = lib.mkMerge [
    {
      home.packages = with pkgs; [
        nodePackages.cspell
        (python3.withPackages (ps: with ps; [
          matplotlib
        ]))
      ];
    }

    (lib.mkIf (cfg.gui.enable && cfg.gui.displayServer == "x11") {
      home.packages = with pkgs; [
        highlight-pointer
        xdragon
        xdotool
        xclip
        xorg.xmodmap
        xorg.xev
        xorg.xdpyinfo
      ];
    })

    (lib.mkIf (cfg.gui.enable && cfg.gui.displayServer == "wayland") {
      home.packages = with pkgs; [
        wev
        wl-clipboard-rs
      ];
    })
  ];
}
