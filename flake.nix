{
  description = "Your new nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # TODO: Add any other flake you might need
    hardware.url = "github:nixos/nixos-hardware";

    cornelis.url = "github:isovector/cornelis";
    cornelis.inputs.nixpkgs.follows = "nixpkgs";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";
  };

  outputs = 
    { nixpkgs
    , home-manager
    , cornelis
    , ... 
    }@inputs: {
    # NixOS configuration entrypoint
    # Available through 'nixos-rebuild --flake .#your-hostname'
    nixosConfigurations = {
      herring = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [ 
          ./configuration.nix 
          home-manager.nixosModules.home-manager
          {
            nixpkgs.overlays = [ cornelis.overlays.cornelis ];
            # home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.fm = import ./home-herring.nix;
          }
        ];
      };
    };

    # Standalone home-manager configuration entrypoint
    # Available through 'home-manager --flake .#your-username@your-hostname'
    homeConfigurations = {
      "marfre03@e114112" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux; 
        extraSpecialArgs = { inherit inputs; }; 
        modules = [ ./home-e114112.nix ];
      };
    };


    # homeConfigurations = {
    #   "fm@herring" = home-manager.lib.homeManagerConfiguration {
    #     pkgs = nixpkgs.legacyPackages.x86_64-linux; 
    #     extraSpecialArgs = { inherit inputs; }; 
    #     modules = [ ./home-herring.nix ];
    #   };
    # };
  };
}
