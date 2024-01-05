{
  programs.zsh = {
    enable = true;
    # fish-like autosuggestion
    enableAutosuggestions = true;
    # Enable zsh syntax highlighting
    syntaxHighlighting.enable = true;
    # util functions
    initExtra = builtins.readFile ./zshrc.zsh;
    # Add user completions to fpath
    initExtraBeforeCompInit = ''
      fpath+=(~/.zsh_completions)
    '';
  };
}

