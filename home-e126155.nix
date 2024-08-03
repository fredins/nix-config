{ pkgs, ... }: 
{
  imports = [
    ./nvim
    ./xmonad
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [ ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  nix = {
    package = pkgs.nix;
    settings.experimental-features = [ "nix-command" "flakes" ];
  };

  home = { 
    username = "marfre03";
    homeDirectory = "/home/marfre03";
    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.05";
    packages = with pkgs; [ 
      ghc 
      haskellPackages.hoogle
      cabal-install
      cmake
      nixgl.nixGLIntel
      texlive.combined.scheme-full
    ];
  };

  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;

  manual.html.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

}
