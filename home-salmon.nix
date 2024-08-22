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
    username = "fm";
    homeDirectory = "/home/fm";
    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.05";
    packages = with pkgs; [ 
      lmodern
      inotify-tools
      keepassxc
      nixgl.nixGLIntel
      libsForQt5.oxygen-icons5

      # Haskell stuff
      ghc
      hlint
      haskellPackages.cabal-install
      haskellPackages.stylish-haskell
      haskellPackages.hoogle
      haskellPackages.threadscope
      haskellPackages.BNFC
      haskellPackages.alex
      haskellPackages.happy

      # Agda
      cornelis

      # C++
      cmake

      # Latex
      texlive.combined.scheme-full
      ltex-ls

    ];
  };

  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "fredins";
    userEmail = "fredin.martin@gmail.com";
  };
  programs.ssh.enable = true;
  programs.bash = {
    enable = true;
    initExtra = ''export PS1="┌──[\w] \[\e[1m\]\$(git branch --show-current 2> /dev/null)\[\e[0m\]\n└─\[\e[1m\]λ\[\e[0m\] "'';
    historyControl = ["erasedups"];
    bashrcExtra = ''
      export EDITOR="nvim";
      export TERMINAL="kitty";
      export BROWSER="firefox"
      export PATH=$PATH:/home/fm/.local/bin:/home/fm/.cabal/bin
    '';
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
