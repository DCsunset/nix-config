{ pkgs, dc-lib, ... }:

{
  imports = dc-lib.importSubdirs ./.;

  # language servers
  config = {
    home.packages = with pkgs; [
      # language servers (used by helix and emacs)
      clang-tools
      # for HTML/CSS/JSON/ESLint language servers
      vscode-langservers-extracted
      # emacs lsp-mode relies on typescript package
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.bash-language-server
      nodePackages.yaml-language-server
      nodePackages.dockerfile-language-server-nodejs
      python3Packages.python-lsp-server
      haskell-language-server
      rust-analyzer
      taplo
      gopls
      nil
      texlab
      lua-language-server

      vim
    ];

    home.sessionVariables = {
      # default editor for root or when emacs is not enabled
      EDITOR = "hx";
    };
  };
}
