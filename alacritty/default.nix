{ pkgs, ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        padding.x = 10;
        padding.y = 10;
      };
      font = {
        normal.family = "DejaVu Sans Mono";
        normal.style = "Regular";
        size = 8.0;
      };
      colors = {
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
    };
  };
}

