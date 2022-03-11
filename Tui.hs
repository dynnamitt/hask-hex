module Main where

import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events

import Materials
import FiniteHexGrid
import InfiniteHexGrid

import System.Random (getStdGen,mkStdGen)
import Data.List (intersperse)

mat = fromID materialPacks "mono"
seed = 2022

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App (IHexGrid Int) e ()
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IHexGrid Int
buildInitialState =
  initIHexGrid gen (0, materialSpan)
  where
    materialSpan = length mat - 1
    gen = mkStdGen seed

drawTui :: IHexGrid Int -> [Widget ()]
drawTui grid =
  [ borderWithLabel (str " [q]uit | wasd ") $
    vBox $ (map str $ finiteHexGridZ viewPort grid)
    ]
  where
    viewPort = ViewPort (150, 150) (15,15) _zoom mat
    _zoom = 2

handleTuiEvent :: IHexGrid Int -> BrickEvent n e -> EventM n (Next (IHexGrid Int))
handleTuiEvent state (VtyEvent vtye) =
    case vtye of
      EvKey (KChar 'q') [] -> halt state
      EvKey (KChar 'w') [] -> continue (move North state)
      EvKey (KChar 'a') [] -> continue (move West state)
      EvKey (KChar 's') [] -> continue (move South state)
      EvKey (KChar 'd') [] -> continue (move East state)
      _ -> continue state
handleTuiEvent state _ = continue state

main :: IO ()
main = do
  let initialState = buildInitialState
  finalState <- defaultMain tuiApp initialState
  return ()
