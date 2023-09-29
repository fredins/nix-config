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
    powerline-fonts
    courier-prime
    julia-mono
    lmodern

    # System and utilities
    lxqt.lxqt-themes
    libsForQt5.oxygen-icons5
    xclip
    dmenu
    powertop
    ripgrep
    ncdu
    pciutils
    yt-dlp
    spotdl
    ffmpeg
    pandoc
    magic-wormhole
    htop
    gnumake
    graphviz

    # Applications
    feh
    qpwgraph
    firefox
    okular
    spotify
    discord
    thunderbird
    zoom-us
    mpv
    qbittorrent
    inotify-tools
    keepassxc

    # LLVM
    (llvmPackages_15.libllvm.override{debugVersion = true;})
    llvmPackages_15.clangUseLLVM
    lldb_15
    
    # Haskell and Agda
    ghc
    haskell-language-server
    hlint
    haskellPackages.cabal-install
    haskellPackages.stylish-haskell
    haskellPackages.hoogle
    haskellPackages.fix-whitespace
    (agda.withPackages [ agdaPackages.standard-library ])
    cornelis

    # Other
    ltex-ls
    texlive.combined.scheme-full
  ];

  programs.neovim.enable = true;
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
    export BROWSER="firefox"
    export TERMINAL="alacritty"
    export PATH=$PATH:/home/fm/.local/bin:/home/fm/.cabal/bin
  '';

  programs.alacritty.enable = true;
  programs.alacritty.settings.window.padding.x = 10;
  programs.alacritty.settings.window.padding.y = 10;
  programs.alacritty.settings.font = {
    # normal.family = "Courier Prime";
    # normal.family = "Julia Mono";
    normal.family = "DejaVu Sans Mono";

    normal.style = "Regular";
    size = 8.0;
  };
  programs.alacritty.settings.colors = {
    primary = {
      background = "0xffffff"; # "0xeeeeee";
      foreground = "0x111111"; #0x444444
    };

    cursor = {
      text = "0xeeeeee";
      cursor = "0x444444";
    };

    normal = {
      black = "0xeeeeee";
      red = "0xaf0000";
      green = "0x008700";
      yellow = "0x5f8700";
      blue = "0x0087af";
      magenta = "0x878787";
      cyan = "0x005f87";
      white = "0x444444";
    };

    bright = {
      black = "0xbcbcbc";
      red = "0xd70000";
      green = "0xd70087";
      yellow = "0x8700af";
      blue = "0xd75f00";
      magenta = "0xd75f00";
      cyan = "0x005faf";
      white = "0x005f87";
    };
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.base  
      haskellPackages.containers  
      haskellPackages.xmonad-contrib  
    ];
  };

  services.picom.enable = true;
  services.picom.shadow = true;
  services.picom.settings = {
    clip-shadow-above = "g:a:lxqt-panel";
  };

  manual.html.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.05";
}
