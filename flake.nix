{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur-dcsunset = {
      url = "github:DCsunset/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, nur-dcsunset, home-manager, ... }: let
    cfg = import ./config.nix;
  in {
    homeConfigurations.${cfg.user} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs { inherit (cfg) system; };
      extraSpecialArgs = {
        dc-lib = nur-dcsunset.lib;
      };

      modules = [
        {
          nixpkgs = {
            overlays = [ nur-dcsunset.overlays.pkgs ];
            config.allowUnfree = true;
          };

          home.username = cfg.user;
          home.homeDirectory = "/home/${cfg.user}";
          home.stateVersion = "24.11";

          programs.home-manager.enable = true;
        }
        ./home.nix
      ] ++ cfg.modules;
    };
  };
}
