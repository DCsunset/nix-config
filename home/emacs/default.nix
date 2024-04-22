{ pkgs, config, lib, mylib, ... }:

{
  programs.emacs = {
    enable = true;
    package = with pkgs; emacs29;
    # emacs packages
    extraPackages = epkgs: with epkgs; [
      openwith
      rg
      dired-sidebar
      valign
      shrink-path
      expand-region
      uuidgen
      jtsx
      pkgs.nur-dcsunset.emacsPackages.modaled
      pkgs.nur-dcsunset.emacsPackages.combobulate
      pkgs.nur-dcsunset.emacsPackages.org-moderncv
      multiple-cursors
      default-text-scale
      sideline
      sideline-flymake
      flymake-cspell
      popwin
      minions
      csv-mode
      beancount
      rainbow-mode
      dashboard
      xclip
      modus-themes
      centaur-tabs
      nerd-icons
      nerd-icons-dired
      nerd-icons-completion
      projectile
      which-key
      vertico
      marginalia
      orderless
      highlight
      vterm
      vterm-toggle
      diff-hl
      magit
      magit-todos
      dash
      hl-todo
      markdown-mode
      nushell-mode
      org
      org-roam
      org-roam-ui
      org-super-agenda
      xeft
      company
      nix-mode
      haskell-mode
      caddyfile-mode
      # tree-sitter for emacs 29+
      (treesit-grammars.with-grammars (grammars: with grammars; [
        tree-sitter-json
        tree-sitter-yaml
        tree-sitter-toml
        tree-sitter-html
        tree-sitter-css
        tree-sitter-markdown
        tree-sitter-make
        tree-sitter-dockerfile
        tree-sitter-python
        tree-sitter-bash
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-go
        tree-sitter-gomod
        tree-sitter-rust
        tree-sitter-javascript
        tree-sitter-typescript
        tree-sitter-tsx
        tree-sitter-nix
        tree-sitter-elisp
      ]))
    ];
    extraConfig = mylib.readFiles [
      ./common.el
      ./hx.el
      ./modeline.el
      ./language.el
      ./tempo.el
      ./check.el
      ./org.el
      ./dir.el
      ./default.el
    ];
  };

  # some config only take effects in user config instead of default.el
  # home.file.".config/emacs/init.el".source = ./init.el;

  home.shellAliases = {
    # emacs in terminal
    et = "emacsclient -t";
    # emacs with GUI
    eg = "emacsclient -c";
  };

  home.activation = {
    emacsInit = lib.hm.dag.entryAfter ["writeBoundary"] ''
      mkdir=${pkgs.coreutils}/bin/mkdir
      emacsDir=/home/${config.home.username}/.config/emacs
      # create necessary dirs
      $DRY_RUN_CMD mkdir -p $emacsDir/auto-saves $emacsDir/backups
    '';
  };

  # enable emacs daemon
  services.emacs.enable = true;
}
