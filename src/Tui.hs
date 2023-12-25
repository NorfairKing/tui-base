{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap             (attrMap)
import Brick.Main                (App(..), continueWithoutRedraw, defaultMain, halt, showFirstCursor)
import Brick.Types               (BrickEvent(VtyEvent), EventM, Widget)
import Brick.Widgets.Core        ()
import Graphics.Vty.Attributes   (defAttr)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar))

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState =
  TuiState
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw         = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleTuiEvent
    , appStartEvent   = pure ()
    , appAttrMap      = const $ attrMap defAttr []
    }

buildInitialState :: IO TuiState
buildInitialState = pure TuiState

drawTui :: TuiState -> [Widget ResourceName]
drawTui _ts = []

handleTuiEvent :: BrickEvent n e -> EventM n s ()
handleTuiEvent e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt
        _ -> continueWithoutRedraw
    _ -> continueWithoutRedraw
