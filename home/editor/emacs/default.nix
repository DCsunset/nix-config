{ config, pkgs, lib, mylib, ... }:

{
  programs.emacs = {
    enable = true;
    package = with pkgs; emacs29;
    # emacs packages
    extraPackages = epkgs: with epkgs; [
      ## common.el
      dash
      nerd-icons
      pkgs.nur-dcsunset.emacsPackages.modaled
      pkgs.nur-dcsunset.emacsPackages.kkp
      # pkgs.custom.modaled
      # pkgs.custom.kkp

      ## check.el
      sideline
      sideline-flymake
      flymake-cspell

      ## org.el
      org
      org-roam
      org-roam-ui
      org-super-agenda
      pkgs.nur-dcsunset.emacsPackages.org-moderncv
      valign

      # language.el
      pkgs.nur-dcsunset.emacsPackages.combobulate
      jtsx
      beancount
      nix-mode
      markdown-mode
      nushell-mode
      haskell-mode
      caddyfile-mode
      lua-mode

      # modeline.el
      shrink-path

      # tempo.el
      uuidgen

      # hx.el
      expand-region
      multiple-cursors
      default-text-scale
      popwin

      # dir.el
      openwith
      dired-sidebar
      nerd-icons-dired
      projectile
      consult
      envrc
      rg

      # ai.el
      ellama

      # misc
      undo-fu
      vundo
      minions
      csv-mode
      rainbow-mode
      dashboard
      xclip
      modus-themes
      centaur-tabs
      nerd-icons-completion
      which-key
      vertico
      marginalia
      orderless
      highlight
      vterm
      vterm-toggle
      diff-hl
      hl-todo
      blamer
      magit
      magit-todos
      xeft
      company
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
    # add the directory to nix store as lua-language-server will try to watch the config dir
    # otherwise it uses high cpu since /nix/store is large
    extraConfig =
      builtins.replaceStrings
        [ "@lua-language-server-config@" ]
        [ "${./lsp-config}/lua-language-server-config.lua" ]
        (mylib.readFiles [
          ./common.el
          ./hx.el
          ./modeline.el
          ./tempo.el
          ./check.el
          ./org.el
          ./language.el
          ./dir.el
          ./ai.el
          ./default.el
        ]);
  };
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
  services.emacs.enable = true;

  # create necessary dirs
  systemd.user.tmpfiles.rules = let
    emacsDir = "/home/${config.home.username}/.config/emacs";
  in [
    "d ${emacsDir}/auto-saves - - - -"
    "d ${emacsDir}/backups - - - -"
  ];
}

