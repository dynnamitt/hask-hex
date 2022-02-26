module Main where

import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events

import Worlds
import FiniteHexGrid
import InfiniteHexGrid

import System.Random (getStdGen,mkStdGen)
import Data.List (intersperse)

w' = worldFromName worlds "mono"

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
  initIHexGrid gen (0, wSize w')
  where
    gen = mkStdGen $ wSeed w'

drawTui :: IHexGrid Int -> [Widget ()]
drawTui grid =
  [ borderWithLabel (str " [q]uit | wasd ") $
    vBox $ (map str $ finiteHexGrid viewPort grid)
    ]
  where
    viewPort = ViewPort (150, 150) (15,15) zoom w'
    zoom = 2

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
