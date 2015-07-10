module NewTrumpGame.Player
  (Player, initialDraw, playerName, hand, deck) where

import Lens.Family2
import Lens.Family2.Unchecked
import qualified Haste.Perch as P
import Data.Monoid (mconcat, mappend)
import NewTrumpGame.Cards (Card)

data Player = Player
  { _hand :: [Card]
  , _deck :: [Card]
  , _playerName :: String
  , _playerId :: String
  , _representation :: Card -> String }

instance P.ToElem Player where
  toElem p = mappend
    (P.forElems ("#" ++ (p ^. playerId) ++" .deck") $
      mappend P.clear $
        P.toElem $ p ^. playerName ++ "の残り山札: " ++ (show $ length $ p ^. deck))
    (P.forElems ("#" ++ (p ^. playerId) ++" .hand") $
      mappend P.clear $
        mconcat $ map (P.li . (p ^. representation)) $ p ^. hand)

hand :: Lens' Player [Card]; hand = lens _hand $ \p x -> p { _hand = x }
deck :: Lens' Player [Card]; deck = lens _deck $ \p x -> p { _deck = x }
playerName :: Lens' Player String; playerName = lens _playerName $ \p x -> p { _playerName = x }
playerId :: Lens' Player String; playerId = lens _playerId $ \p x -> p { _playerId = x }
representation :: Lens' Player (Card -> String); representation = lens _representation $ \p x -> p { _representation = x }

initialDraw :: String -> String -> (Card -> String) -> [Card] -> Player
initialDraw name plid rep d = Player (take 3 d) (drop 3 d) name plid rep
