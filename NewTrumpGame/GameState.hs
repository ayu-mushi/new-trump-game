{-# LANGUAGE Rank2Types #-}
module NewTrumpGame.GameState
  (initGame, Game, players) where
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

newtype Field = Field { fromField :: [[Maybe Card]] }

instance P.ToElem Field where
  toElem (Field xss) = P.forElems "table#field" $
    mappend P.clear $
      mconcat $ map (P.tr . mconcat . map (P.td . showMaybeCard)) xss
    where
      showMaybeCard mbcard = case mbcard of
        Nothing -> ""
        Just card -> show card

data Phase =
  Draw
  | Hand
  | Sacrifice
    Int -- cost of object of summon
    [Int] -- sucrifices
  | Summon 
    Int 
  | End
  | Finish Bool

instance Show Phase where
  show p = case p of
    Draw          -> "ドロー"
    Hand          -> "手札を選択"
    Sacrifice _ _ -> "生贄を選択"
    Summon    _   -> "召喚する位置を選択"
    End           -> "手番を交代"
    Finish True   -> "あなたの勝ちです!"
    Finish False  -> "コンピュータが勝ち!"

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
selectObjOfSummon i game = let yourHand = game ^. players . _1 . hand in
  if isColored $ yourHand !! i
    then if 0 == (cost $ fromJust $ fromCard $ yourHand !! i)
      then phase .~ Summon i $ game
      else phase .~ Sacrifice i [] $ game
    else game

selectSacrifice :: Int -> Game -> Game
selectSacrifice i game = let yourHand = game ^. players . _1 . hand in
  case game ^. phase of
    Sacrifice costOfObjOfSummon sacrifices ->
      if costOfObjOfSummon < (foldr (+) 0 $ map (energy . (yourHand!!)) $ i:sacrifices)
        then phase .~ (Sacrifice costOfObjOfSummon $ insert i sacrifices) $ game
        else phase .~ End $ players._1.hand %~ (foldr (.) id $ map delByIx sacrifices) $ game
    _ ->
      error "You can select sacrifice if and only if it is sacrifice phase and is your turn"

delByIx :: Int -> [a] -> [a]
delByIx i xs = (take i xs) ++ (drop (i+1) xs)

summonableZone :: Lens' Field [Maybe Card]
summonableZone = lens (last.fromField) $ \(Field p) x -> Field $ (init p) ++ [x]

ix :: Int -> Lens' [a] a
ix i = lens (!! i) $ \p x -> (take i p) ++ [x] ++ (drop (i+1) p)

summon :: Int -> Game -> Game
summon for game = let yourHand = game ^. players . _1 . hand in
  case game ^. phase of
    Summon objOfSummon ->
      if (isNothing $ (last $ fromField $ game ^. field) !! for) && ((cost $ fromJust $ fromCard $ yourHand!!objOfSummon) <= (foldr (+) 0 $ map energy $ yourHand))
        then
          game
            & players . _1 . hand %~ delByIx objOfSummon
            & field . summonableZone . (ix for) .~ (Just $ yourHand!!objOfSummon)
            & phase .~ Sacrifice (cost $ fromJust $ fromCard $ yourHand!!objOfSummon) []
        else 
          error "it is a havitant, previously"
    _ -> error "You can select summon zone if and only if it is summon phase and is your turn"

draw :: Game -> Lens' (Player, Player) Player -> Game
draw game which = let get (a:newDeck) = Just a; get _ = Nothing in
  case get $ game ^. players . which . deck of
    Just card -> game & players . which . hand %~ (card:) & players . which . deck %~ tail
    Nothing   -> game

instance P.ToElem Game where
  toElem game = mconcat [
    P.toElem $ game ^. field
   ,P.forElems "#status" $
      mappend P.clear $
        P.toElem $ "-- " ++ (game ^. turnPlayer . playerName) ++ "の番です、" ++ (show $ game ^. phase)
   ,uncurry mappend $ both %~ P.toElem $ game ^. players
   ,case game ^. phase of
      Sacrifice costOfObjOfSummon objOfSacr ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e "#yours ol.hand li"
          mapM_ (setAttr `flip` "class" `flip` "sacrifice") $ map (handsLis !!) objOfSacr
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
      (initialDraw "あなた" "yours" show $ initDeck g,
        initialDraw "コンピュータ" "computers" (const "?") $ initDeck h)
    , _areYouTurnPlayer = True
    , _phase = Summon 2
    , _field = Field $ replicate 5 (replicate 3 Nothing)
  }
