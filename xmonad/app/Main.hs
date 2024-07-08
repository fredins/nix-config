{-# LANGUAGE OverloadedRecordDot #-}      
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}

module Main where

-- import XMonad 
import XMonad  
  ( xmonad
  , XConfig
  , Choose(..)
  , Full(..)
  , Mirror(..)
  , (|||)
  , (.|.)
  )
import qualified XMonad
import XMonad.Layout.ResizableTile (ResizableTall(..), MirrorResize (MirrorShrink, MirrorExpand))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified XMonad.Hooks.EwmhDesktops as Hooks
import qualified XMonad.Hooks.ManageDocks as Hooks
import qualified XMonad.StackSet as StackSet

data Keybind = MkKeyBind
  { mask :: XMonad.KeyMask
  , sym  :: XMonad.KeySym
  , action   :: XMonad.X ()
  }

keys :: XConfig XMonad.Layout -> Map (XMonad.KeyMask, XMonad.KeySym) (XMonad.X ())
keys config = Map.fromList $ map (\ key -> ((key.mask, key.sym), key.action)) $
  [ MkKeyBind 
      do config.modMask .|. XMonad.shiftMask 
      do XMonad.xK_c 
      do XMonad.kill

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_space 
      do XMonad.sendMessage XMonad.NextLayout

  , MkKeyBind 
      do config.modMask .|. XMonad.shiftMask 
      do XMonad.xK_space 
      do XMonad.setLayout config.layoutHook

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_r
      do XMonad.refresh

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_j
      do XMonad.windows StackSet.focusDown

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_k
      do XMonad.windows StackSet.focusUp

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_m
      do XMonad.windows StackSet.focusMaster

  , MkKeyBind 
      do config.modMask 
      do XMonad.xK_Return
      do XMonad.windows StackSet.swapMaster

  , MkKeyBind 
      do config.modMask .|. XMonad.shiftMask
      do XMonad.xK_j
      do XMonad.windows StackSet.swapDown

  , MkKeyBind 
      do config.modMask .|. XMonad.shiftMask
      do XMonad.xK_j
      do XMonad.windows StackSet.swapUp

  , MkKeyBind 
      do config.modMask
      do XMonad.xK_h
      do XMonad.sendMessage XMonad.Shrink

  , MkKeyBind 
      do config.modMask
      do XMonad.xK_l
      do XMonad.sendMessage XMonad.Expand

  , MkKeyBind 
      do config.modMask .|. XMonad.shiftMask
      do XMonad.xK_h
      do XMonad.sendMessage MirrorShrink


  , MkKeyBind 
      do config.modMask .|. XMonad.shiftMask
      do XMonad.xK_l
      do XMonad.sendMessage MirrorExpand

  , MkKeyBind 
      do config.modMask
      do XMonad.xK_t
      do XMonad.withFocused $ XMonad.windows . StackSet.sink

  , MkKeyBind 
      do config.modMask
      do XMonad.xK_comma
      do XMonad.sendMessage (XMonad.IncMasterN 1)

  , MkKeyBind 
      do config.modMask
      do XMonad.xK_period
      do XMonad.sendMessage (XMonad.IncMasterN -1)
  ] ++

  [ MkKeyBind 
      do config.modMask .|. mask 
      do key 
      do XMonad.windows (f workspace)
  | (workspace, key) <- zip config.workspaces [XMonad.xK_1 .. XMonad.xK_9]
  , (f, mask) <- [(StackSet.greedyView, 0), (StackSet.shift, XMonad.shiftMask)]
  ] ++
  
  [ MkKeyBind 
      do config.modMask .|. mask
      do key 
      do XMonad.screenWorkspace screen >>= flip XMonad.whenJust (XMonad.windows . f)
  | (key, screen) <- zip [XMonad.xK_w, XMonad.xK_e, XMonad.xK_r] [0..]
  , (f, mask) <- [(StackSet.view, 0), (StackSet.shift, XMonad.shiftMask)]
  ]

type Layout = Choose ResizableTall (Choose (Mirror ResizableTall) Full)

layoutHook :: Layout a
layoutHook = layout

layout :: Layout a
layout = tiled ||| Mirror tiled ||| Full
  where
  tiled = ResizableTall 1 (3/100) (1/2) []

config :: XConfig Layout
config = XMonad.def 
  { XMonad.workspaces        = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  , XMonad.modMask           = XMonad.mod4Mask
  , XMonad.focusFollowsMouse = False
  , XMonad.keys              = keys
  , XMonad.layoutHook        = layoutHook
  }

main :: IO ()
main = xmonad 
      $ Hooks.ewmh
      $ Hooks.ewmhFullscreen 
      $ Hooks.docks
      $ config



