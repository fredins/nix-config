# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, lib, config, pkgs, ... }: 
{
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      inputs.cornelis.overlays.cornelis
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  home.username = "fm";
  home.homeDirectory = "/home/fm";
  home.packages = with pkgs; [
    # Fonts
    lmodern

    # Applications and tools
    inotify-tools
    keepassxc

    # Haskell and Agda
    ghc
    haskell-language-server
    hlint
    haskellPackages.cabal-install
    haskellPackages.stylish-haskell
    haskellPackages.hoogle
    (agda.withPackages [ agdaPackages.standard-library ])
    cornelis

    # Other
    ltex-ls
    texlive.combined.scheme-full

    nodePackages.grammarly-languageserver
  ];

  # programs.neovim.enable = true;
  # programs.neovim.extraLuaPackages = [ pkgs.lua51Packages.luautf8 ];
  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "fredins";
    userEmail = "fredin.martin@gmail.com";
  };

  programs.ssh.enable = true;
  programs.ssh.matchBlocks = {
    "fujutsi" = {
      hostname = " 188.149.179.247";
      user = "fm";
      identityFile = ["~/.ssh/id_ed25519"];
    };
    "fujutsi_local" = {
      hostname = "192.168.68.124";
      user = "fm";
      identityFile = ["~/.ssh/id_ed25519"];
    };
  };
  programs.bash.enable = true;
  programs.bash.shellAliases = {
    v = "nvim";
  };
  programs.bash.initExtra = ''
    export EDITOR="nvim"
    export BROWSER="firefox-bin"
    export TERMINAL="alacritty"
    export PATH=$PATH:/home/fm/.local/bin:/home/fm/.cabal/bin
  '';

  manual.html.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.05";
}
