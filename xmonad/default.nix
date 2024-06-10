{ pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    config = ./xmonad.hs;
    enableContribAndExtras = true;
    extraPackages = haskellPackages: [
      haskellPackages.containers  
      haskellPackages.xmonad-contrib  
    ];
    libFiles = {
      "IfMaxAlt.hs" = lib/IfMaxAlt.hs;
      "SideBorderDecoration.hs" = lib/SideBorderDecoration.hs;
    };
  };
}
