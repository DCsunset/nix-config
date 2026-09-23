{ config, pkgs, lib, dc-lib, ... }:

let
  cfg = config.dc-home;
in
{
  imports = dc-lib.importSubdirs ./.;

  options.dc-home = {
    gui = {
      enable = lib.mkEnableOption "gui apps";
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
        fastfetch
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
          # HACK: wait until https://github.com/NixOS/nixpkgs/pull/552741 is merged
          # scienceplots
          numpy
          scipy
          pandas
          psutil
          requests
        ] ++ requests.optional-dependencies.socks))
        cspell
        go
        nodejs
      ];

      home.file.".npmrc".text = ''
        prefix = ''${HOME}/.npm
      '';

      home.sessionPath = [
        "${config.home.homeDirectory}/.npm/bin"
        "${config.home.homeDirectory}/go/bin"
      ];
    }

    (lib.mkIf cfg.gui.enable {
      home.packages = with pkgs; [
        wev
        wl-clipboard-rs
      ];
    })
  ];
}
