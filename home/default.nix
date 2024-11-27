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
        pueue
        iproute2
        nix-tree
        bottom
        htop
        openssh
        socat
        websocat
        dig
        neofetch
        tokei
        ripgrep
        fd
        sqlite
        xh
        wget
        curl
        dufs
        ranger
        (python3.withPackages (ps: with ps; [
          matplotlib
        ]))
        nodePackages.cspell
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
