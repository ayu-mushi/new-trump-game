module Game.BoardTrump.CPU () where

import Lens.Family2
import Lens.Family2.Stock (_2)
import Data.Maybe (fromJust)

import Game.BoardTrump.Player (hand)
import Game.BoardTrump.GameState

newtype Play = Play (Either Int (Int, Int))

advantage :: Game -> Int
advantage = undefined

possiblePlay :: Game -> [Play]
possiblePlay game =
  case game ^. phase of
    Main -> undefined
    Sacrifice cost -> map (Play . Left) [0..(pred $ length $ game ^. turnPlayer . hand)]
    Move (x, y) -> map (Play . Right) $ movableZone (view _2 $ fromJust $ game ^. field . cell x y) game x y
    Summon obj -> undefined

randomly :: Game -> Play
randomly = undefined
