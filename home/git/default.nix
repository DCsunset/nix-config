{ ... }:

{
  programs.git = {
    enable = true;
    aliases = {
      st = "status";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
    };
    ignores = [
      # npm
      "node_modules"
      # python
      "__pycache__"
      "*.egg-info"
      # emacs temp files
      "*~"
      ".#*"
    ];
    # Use https://github.com/dandavison/delta for diff
    delta = {
      enable = true;
      options = {
        # use n and N to move between diff sections
        navigate = true;
        line-numbers = true;
      };
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
      # allow using file as url
      protocol.file.allow = "always";
    };
  };
}

