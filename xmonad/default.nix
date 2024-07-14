{ pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    config = ./app/Main.hs;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: with pkgs.haskellPackages; [
      containers  
      xmonad-contrib  
    ];
    #   libFiles = {
    #     "IfMaxAlt.hs" = lib/IfMaxAlt.hs;
    #     "SideBorderDecoration.hs" = lib/SideBorderDecoration.hs;
    #   };
  };
}
