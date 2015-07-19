module NewTrumpGame.Player
  (Player, initialDraw, playerName, hand, deck, playerId) where

import Lens.Family2
import Lens.Family2.Unchecked
import qualified Haste.Perch as P
import Data.Monoid (mconcat, mappend)
import NewTrumpGame.Cards (Card, Color)

data Player = Player
  { _hand :: [Card]
  , _deck :: [Card]
  , playerName :: String
  , playerId :: String
  , representation :: Card -> String}

instance P.ToElem Player where
  toElem p = mappend
    (P.forElems ("#" ++ (p & playerId) ++" .deck") $
      mappend P.clear $
        P.toElem $ (p & playerName) ++ "の残り山札: " ++ (show $ length $ p ^. deck))
    (P.forElems ("#" ++ (p & playerId) ++" .hand") $
      mappend P.clear $
        mconcat $ map (P.li . (p & representation)) $ p ^. hand)

hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x }
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x }

initialDraw :: String -> String -> (Card -> String) -> [Card] -> Player
initialDraw name plid rep d = Player (take 3 d) (drop 3 d) name plid rep
