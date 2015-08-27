module Game.BoardTrump.CPU (randomly) where

import Lens.Family2
import Lens.Family2.Stock (_2)
import Data.Maybe (fromJust, mapMaybe, catMaybes, isNothing, isJust)
import System.Random (Random(randomR), StdGen)

import Game.BoardTrump.Player (hand)
import Game.BoardTrump.Core
import Game.BoardTrump.Util (ix, forPlaneWithIx)

advantage :: Game -> Int
advantage = undefined

possiblePlay :: Game -> [Play]
possiblePlay game = Pass:
  (case game ^. phase of
    Main ->
      (map WithField $ catMaybes $ concat $ forPlaneWithIx (game ^. field) $ \i j m -> case m of Just (p, c) -> if (p == (game ^. isYourTurn)) && (not $ null $ movableZone c game i j) then Just (i, j) else Nothing; Nothing -> Nothing)
      ++ (if all isJust $ game ^. field . summonableZone (game ^. isYourTurn) then [] else (map (WithHand) $ mapMaybe (\(i, p) -> if p then Just i else Nothing) $ zip [0..] $ map (isSummonable game) $ game ^. turnPlayer . hand))
    Sacrifice cost -> map (WithHand) [0..(pred $ length $ game ^. turnPlayer . hand)]
    Move (x, y) -> map (WithField) $ movableZone (view _2 $ fromJust $ game ^. field . cell x y) game x y
    Summon obj -> map (WithField . (,) (if game^.isYourTurn then pred $ length $ game ^. field else 0)) $ mapMaybe (\(i, m) -> if isNothing m then Just i else Nothing) $ zip [0..] $ game ^. field . summonableZone (game ^. isYourTurn)
    _ -> [])

randomly :: Game -> (Play, StdGen)
randomly game =
  let (i, g) = randomR (0, pred $ length $ possiblePlay game) $ game ^. gen
   in (possiblePlay game ^. ix i, g)
