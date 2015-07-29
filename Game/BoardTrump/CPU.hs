module Game.BoardTrump.CPU (randomly, runPlay) where

import Lens.Family2
import Lens.Family2.Stock (_2)
import Data.Maybe (fromJust, mapMaybe, catMaybes, isJust)
import System.Random (Random(randomR), StdGen)

import Game.BoardTrump.Player (hand)
import Game.BoardTrump.GameState
import Game.BoardTrump.Util (ix, forPlaneWithIx)

newtype Play = Play (Either Int (Int, Int)) deriving Show

advantage :: Game -> Int
advantage = undefined

possiblePlay :: Game -> [Play]
possiblePlay game =
  case game ^. phase of
    Main ->
      (map (Play . Right) $ catMaybes $ concat $ forPlaneWithIx (game ^. field) $ \i j m -> case m of Just (p, c) -> if (p == (game ^. isYourTurn)) && (not $ null $ movableZone c game i j) then Just (i, j) else Nothing; Nothing -> Nothing)
      ++ (map (Play . Left) $ mapMaybe (\(i, p) -> if p then Just i else Nothing) $ zip [0..] $ map (isSummonable game) $ game ^. turnPlayer . hand)
    Sacrifice cost -> map (Play . Left) [0..(pred $ length $ game ^. turnPlayer . hand)]
    Move (x, y) -> map (Play . Right) $ movableZone (view _2 $ fromJust $ game ^. field . cell x y) game x y
    Summon obj -> map (Play . Right . (,) (if game^.isYourTurn then pred $ length $ game ^. field else 0)) $ mapMaybe (\(i, m) -> if isJust m then Just i else Nothing) $ zip [0..] $ game ^. field . (summonableZone $ game ^. isYourTurn)
    _ -> []

randomly :: Game -> (Play, StdGen)
randomly game =
  let (i, g) = randomR (0, pred $ length $ possiblePlay game) $ game ^. gen
   in (possiblePlay game ^. ix i, g)

runPlay :: Play -> Game -> Game
runPlay (Play play) game = case play of
  Left i -> operateWithHand i game
  Right (i, j) -> operateWithField i j game
