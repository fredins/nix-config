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
    sessionVariables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
    };
  
    packages = with pkgs; [ 
      ghc 
      haskellPackages.hoogle
      cabal-install
      cmake
      nixgl.nixGLIntel
    ];
  };

  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;

  programs.bash = {
    enable = true;
    initExtra = ''export PS1="┌──[\w]\$(__git_ps1 \" \[\e[1m\]%s\[\e[0m\]\")\n└─\[\e[1m\]λ\[\e[0m\] "'';
    historyControl = ["erasedups"];
    bashrcExtra = ''
      export EDITOR="nvim";
      export TERMINAL="kitty";
    '';
  };

  manual.html.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

}
