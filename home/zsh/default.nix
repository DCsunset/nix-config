{ lib, config, ... }:

{
  programs.zsh = {
    enable = true;
    dotDir = "${config.xdg.configHome}/zsh";
    # fish-like autosuggestion
    autosuggestion.enable = true;
    # Enable zsh syntax highlighting
    syntaxHighlighting.enable = true;
    initContent = lib.mkMerge [
      # Add user completions to fpath (before completion init)
      (lib.mkOrder 550 ''
        fpath+=(~/.zsh_completions)
      '')

      # shell settings
      (builtins.readFile ./zshrc.zsh)
    ];
  };
}

