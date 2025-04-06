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
        parallel
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
        tmux
        lean4
        (python3.withPackages (ps: with ps; [
          pyyaml
          matplotlib
          scienceplots
          numpy
          scipy
          pandas
          psutil
          requests
        ] ++ requests.optional-dependencies.socks))
        nodePackages.cspell
        go
        nodejs
      ];

      home.file.".npmrc".text = ''
        prefix = ''${HOME}/.npm
      '';
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
