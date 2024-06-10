{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          devShell = {
            enable = true;
            tools = hp: { 
              # xmonad = hp.containers;
              # xmonad = hp.xmonad-contrib;
              # xmonad = hp.xmonad;
            };
          };
        };
        packages.default = self'.packages.xmonad-config;
      };
    };
}
