
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

  home = { 
    username = "fm";
    homeDirectory = "/home/fm";

    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.05";
  
    packages = with pkgs; [
      # Python
      # (python3.withPackages (ps: with ps; [ numpy matplotlib pandas notebook scikit-learn]))


      # Fonts
      # lmodern

      # System and utilities
      lxqt.lxqt-themes
      libsForQt5.oxygen-icons5
      dmenu
      powertop
      ncdu
      pciutils
      # yt-dlp
      # spotdl
      ffmpeg
      pandoc
      magic-wormhole
      htop
      gnumake
      graphviz
      inotify-tools

      # Applications
      # fastfetch
      kitty
      feh
      qpwgraph
      firefox
      okular
      spotify
      # discord
      thunderbird
      zoom-us
      mpv
      qbittorrent
      keepassxc

      # LLVM
      # (llvmPackages_15.libllvm.override{debugVersion = true;})
      llvmPackages_15.clangUseLLVM
      # lldb_15
      
      # Haskell and Agda
      ghc
      hlint
      haskellPackages.cabal-install
      haskellPackages.stylish-haskell
      haskellPackages.hoogle
      # (agda.withPackages [ agdaPackages.standard-library ])

      # Other
      ltex-ls
      texlive.combined.scheme-full
    ];
  };


  # xsession.windowManager.xmonad.enable = true;


  programs.command-not-found.enable = true;
  programs.home-manager.enable = true;
  programs.git = {
    enable = true;
    userName = "fredins";
    userEmail = "fredin.martin@gmail.com";
    extraConfig = {
      pull.rebase = true;
    };
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
