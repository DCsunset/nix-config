{ config, pkgs, lib, dc-lib, ... }:

let
  cfg = config.dc-home;

  kind = if cfg.gui.enable then "gui" else "nox";
  emacsPkg = pkgs.custom.emacs-dcsunset.${kind};
in
{
  home.packages = [ emacsPkg ];

  # Extra config that only works in home init.el
  home.file.".config/emacs/init.el".text = ''
    ;; Disable startup screen
    (setq inhibit-startup-screen t)
  '';

  # some config only take effects in user config instead of default.el
  # home.file.".config/emacs/init.el".source = ./init.el;

  home.shellAliases = {
    # emacs in terminal
    et = "emacsclient -t";
    # emacs with GUI
    eg = "emacsclient -c";
  };

  # enable emacs daemon
  services.emacs = {
    enable = true;
    package = emacsPkg;
  };
}

