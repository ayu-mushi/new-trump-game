{-# LANGUAGE Rank2Types #-}
module NewTrumpGame.GameState
  (initGame, Game, players, draw, summon, selectSacrifice, selectObjOfSummon) where
import Data.Monoid (mconcat, mempty, (<>), mappend)
import Data.List (insert)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2, both)
import System.Random (RandomGen)
import Control.Monad (forM_, when)
import Data.Maybe (isNothing, fromJust)

import NewTrumpGame.Cards
import NewTrumpGame.Player

newtype Field = Field { fromField :: [[Maybe (Either Card Card)]] } -- Left is あなた

instance P.ToElem Field where
  toElem (Field xss) = P.forElems "table#field" $
    mappend P.clear $
      mconcat $ map (P.tr . mconcat . map showCard) xss
    where
      showCard mbcard = case mbcard of
        Nothing -> P.td ""
        Just card -> case card of
          Left a  -> (P.td $ show a) `P.attr` P.atr "class" "your-card"
          Right b -> (P.td $ show b) `P.attr` P.atr "class" "computers-card"

data Phase =
  Draw
  | Main
  | Move
    Int -- subject of moving
  | Sacrifice
    Int -- cost of object of summon
    [Int] -- sucrifices
  | Summon 
    Int -- object of summon
  | End
  | Finish (Maybe Bool)

instance Show Phase where
  show p = case p of
    Draw          -> "ドロー"
    Main          -> "行動を選択"
    Move      _   -> "移動する位置を選択"
    Sacrifice _ _ -> "生贄を選択"
    Summon    _   -> "召喚する位置を選択"
    End           -> "手番を交代"
    Finish Nothing       -> "引き分け!!!!!!!!!!!!!"
    Finish (Just True)   -> "あなたの勝ちです!"
    Finish (Just False)  -> "コンピュータが勝ち!"

data Game = Game {
  _players :: (Player, Player), -- (players ^. _1 . playerName) == "あなた"
  _areYouTurnPlayer :: Bool,
  _phase :: Phase,
  _field :: Field
  }
players :: Lens' Game (Player, Player); players = lens _players (\p x -> p { _players = x })
areYouTurnPlayer :: Lens' Game Bool; areYouTurnPlayer = lens _areYouTurnPlayer $ \p x -> p { _areYouTurnPlayer = x }
phase :: Lens' Game Phase; phase = lens _phase $ \p x -> p { _phase = x }
field :: Lens' Game Field; field = lens _field (\p x -> p { _field = x})

turnPlayer :: Lens' Game Player
turnPlayer = lens getting setting
  where
    getting game = game ^. players . (if game ^. areYouTurnPlayer then _1 else _2)
    setting game p = players . (if game ^. areYouTurnPlayer then _1 else _2) .~ p $ game

selectObjOfSummon :: Int -> Game -> Game
selectObjOfSummon i game = let theHand = game ^. turnPlayer . hand in
  if isColored $ theHand !! i
    then phase .~ Summon i $ game
    else game

selectSacrifice :: Int -> Game -> Game
selectSacrifice i game = let theHand = game ^. turnPlayer . hand in
  case game ^. phase of
    Sacrifice costOfObjOfSummon sacrifices ->
      if costOfObjOfSummon < (foldr (+) 0 $ map (energy . (theHand!!)) $ i:sacrifices)
        then phase .~ (Sacrifice costOfObjOfSummon $ insert i sacrifices) $ game
        else phase .~ End $ turnPlayer.hand %~ (foldr (.) id $ map delByIx sacrifices) $ game
    _ ->
      error "You can select sacrifice if and only if it is sacrifice phase and is your turn"

delByIx :: Int -> [a] -> [a]
delByIx i xs = (take i xs) ++ (drop (i+1) xs)

summonableZone :: Lens' Field [Maybe (Either Card Card)]
summonableZone = lens (last.fromField) $ \(Field p) x -> Field $ (init p) ++ [x]

ix :: Int -> Lens' [a] a
ix i = lens (!! i) $ \p x -> (take i p) ++ [x] ++ (drop (i+1) p)

summon :: Int -> Game -> Game
summon for game = let theHand = game ^. turnPlayer . hand in
  case game ^. phase of
    Summon objOfSummon ->
      if (isNothing $ (last $ fromField $ game ^. field) !! for) && ((cost $ fromJust $ fromCard $ theHand!!objOfSummon) <= (foldr (+) 0 $ map energy $ theHand))
        then
          game
            & turnPlayer . hand %~ delByIx objOfSummon
            & field . summonableZone . (ix for) .~ (Just $ (game^.turnPlayer)&markInField $ theHand!!objOfSummon)
            & (if 0 == (cost $ fromJust $ fromCard $ theHand!!objOfSummon)
              then phase .~ End
              else phase .~ Sacrifice (cost $ fromJust $ fromCard $ theHand!!objOfSummon) [])
        else 
          error "it is a havitant, previously"
    _ -> error "You can select summon zone if and only if it is summon phase and is your turn"

draw :: Game -> Game
draw game = let get (a:newDeck) = Just a; get _ = Nothing in
  case get $ game ^. turnPlayer . deck of
    Just card -> game & turnPlayer . hand %~ (card:) & turnPlayer . deck %~ tail & phase .~ Main
    Nothing   -> game & phase .~ Finish Nothing

instance P.ToElem Game where
  toElem game = mconcat [
    P.toElem $ game ^. field
   ,P.forElems "#status" $
      mappend P.clear $
        P.toElem $ "-- " ++ (game ^. turnPlayer & playerName) ++ "の番です、" ++ (show $ game ^. phase)
   ,uncurry mappend $ both %~ P.toElem $ game ^. players
   ,case game ^. phase of
      Main ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e "#yours ol.hand li"
          let isSelectable card = (isColored card) && ((cost $ fromJust $ fromCard $ card)<=(foldl (+) (0-energy card) $ map energy $ game^.players._1.hand))
          forM_ (zip handsLis $ map isSelectable $ game ^. players . _1 ^. hand) $
            \(eachLi, isItSelectable) -> when isItSelectable $ setAttr eachLi "class" "selectable-hand"
          return e
      Sacrifice costOfObjOfSummon objOfSacr ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e "#yours ol.hand li"
          mapM_ (setAttr `flip` "class" `flip` "sacrifice") $ map (handsLis !!) objOfSacr
          forM_ handsLis $ setAttr `flip` "class" `flip` "selectable-hand" 
          return e
      Summon objOfSummon ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e "#yours ol.hand li"
          setAttr (handsLis !! objOfSummon) "id" "obj-of-summon"
          fieldTrs <- elemsByQS e "#field tr"
          mostUnderTds <- elemsByQS (last fieldTrs) "td"
          forM_ (zip mostUnderTds (map isNothing $ game ^. field . summonableZone)) $
            \(eachTd, isNotLived) -> when isNotLived $ setAttr eachTd "class" "summonable-zone"
          return e
      _ ->
        mempty
    ]

initGame :: RandomGen g => g -> g -> Game
initGame g h = 
  Game {
    _players =
      (initialDraw "あなた" "yours" show Left $ initDeck g,
        initialDraw "コンピュータ" "computers" (const "?") Right $ initDeck h)
    , _areYouTurnPlayer = True
    , _phase = Draw
    , _field = Field $ replicate 5 (replicate 3 Nothing)
  }
