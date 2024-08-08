let
  cfg = import ./config.nix;
in
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

  outputs = inputs@{ nixpkgs, nur-dcsunset, home-manager, ... }: {
    homeConfigurations.${cfg.user} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs { inherit (cfg) system; };
      extraSpecialArgs = {
        dc-lib = nur-dcsunset.lib;

        modules = [
          {
            nixpkgs = {
              overlays = [ nur-dcsunset.overlays.pkgs ];
              allowUnfree = true;
            };
          }
          ./home.nix
        ];
      };
    };
  };
}
