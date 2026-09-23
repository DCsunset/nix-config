{ pkgs, ... }:

let
  osHelpers = import ../../lib/helpers.nix { inherit pkgs; };
in
{
  programs = {
    git = {
      enable = true;
      # use canonical timezone to hide location for privacy
      package = osHelpers.wrapPackage pkgs.git "--set TZ Etc/UTC";
      lfs.enable = true;

      settings = {
        alias = {
          st = "status";
          lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
        };
        # recording of resolved conflicts
        rerere.enabled = true;
        init.defaultBranch = "main";
        # make log --commit-graph faster for large repo
        fetch.writeCommitGraph = true;
        branch.sort = "-committerdate";
        # allow using file as url
        protocol.file.allow = "always";
        # store credentials for HTTP
        # credential.helper = "store --file ${appDataDir}/git-credentials";
      };
      ignores = [
        # direnv
        ".direnv"
        # npm
        "node_modules"
        # python
        "__pycache__"
        "*.egg-info"
        # emacs temp files
        "*~"
        ".#*"
      ];
    };
    delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        # use n and N to move between diff sections
        navigate = true;
        line-numbers = true;
      };
    };
  };
}

