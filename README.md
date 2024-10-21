# nix-config

My person common Nix config.


## Usage

Simply import the module in your home-manager config and
make sure `dc-lib = nur-dcsunset.lib` is added to `extraSpecialArgs` in home-manager modules.

Example:

```nix
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
    dc-nix-config.url = "github:DCsunset/nix-config";
  };

  outputs = { nixpkgs, nur-dcsunset, dc-nix-config, home-manager, ... }: let
    // Replace this
    user = "USER";
  in {
    homeConfigurations.${user}= home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs { system = "x86_64"; };
      extraSpecialArgs = {
        dc-lib = nur-dcsunset.lib;
      };

      modules = [
        dc-nix-config.home
        {
          home.username = user;
          home.homeDirectory = "/home/${user}";
          home.stateVersion = "24.11";

          programs.home-manager.enable = true;

          # dc-home options
          dc-home.gui.enable = true;
        }
      ];
    };
  };
}
```

