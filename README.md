# nix-config

My person common Nix config.


## Usage

Simply import the module in your home-manager config,
make sure the following config is set:
- `dc-lib = nur-dcsunset.lib` is added to `extraSpecialArgs` in home-manager modules
- `nur-dcsunset.overlays.pkgs` is added to nixpkgs overlay.
- `emacs-dcsunset` is added to nixpkgs overlay.

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
    emacs-dcsunset = {
      url = "github:DCsunset/emacs-config";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nur-dcsunset.follows = "nur-dcsunset";
      };
    };
    dc-nix-config.url = "github:DCsunset/nix-config";
  };

  outputs = inputs@{ nixpkgs, nur-dcsunset, dc-nix-config, home-manager, ... }: let
    // Replace this
    user = "USER";
  in {
    homeConfigurations.${user} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        system = "x86_64";
        overlays = [
          nur-dcsunset.overlays.pkgs
          (final: prev: {
            custom = {
              emacs-dcsunset = inputs.emacs-dcsunset.packages.${prev.system};
            };
          })
        ];
      };
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

