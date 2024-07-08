{ pkgs, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    config = ./app/Main.hs;
    enableContribAndExtras = true;
    #   extraPackages = haskellPackages: [
    #     haskellPackages.containers  
    #     haskellPackages.xmonad-contrib  
    #   ];
    #   libFiles = {
    #     "IfMaxAlt.hs" = lib/IfMaxAlt.hs;
    #     "SideBorderDecoration.hs" = lib/SideBorderDecoration.hs;
    #   };
  };
}
