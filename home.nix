{ ... }:

let
  cfg = import ./config.nix;
in
{
  home.username = cfg.user;
  home.homeDirectory = "/home/${cfg.user}";
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;

  imports = [ ./home ];
}
