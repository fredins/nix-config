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
      nixgl.nixGLIntel
      ghc 
      haskellPackages.hoogle
      ncdu
      magic-wormhole
      okular
    ];
  };

  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;

  programs.ssh.enable = true;
  programs.ssh.matchBlocks.machine = { 
    host = "machine";
    hostname = "10.44.11.180";

  };

  services.picom.enable = true;
  services.picom.shadow = true;
  services.picom.settings = {
    clip-shadow-above = "g:a:lxqt-panel";
  };

  manual.html.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
