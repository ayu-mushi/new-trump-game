{-# LANGUAGE Rank2Types #-}
module NewTrumpGame.GameState
  (initGame, Game, Phase(..), selectSbjOfMv, phase, players, draw, summon, move, selectSacrifice, selectObjOfSummon, operateWithHand, operateWithField, areYourTurn) where
import Data.Monoid (mconcat, mempty, (<>), mappend)
import Data.List (insert)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2, both)
import System.Random.Shuffle (shuffle')
import System.Random (StdGen, Random(random))
import Control.Monad (forM_, when, zipWithM_)
import Data.Maybe (isNothing, fromMaybe, isJust, fromJust)

import NewTrumpGame.Cards
import NewTrumpGame.Player

newtype Field = Field { fromField :: [[Maybe (Bool, Card)]] } -- Left is あなた

instance P.ToElem Field where
  toElem (Field xss) = P.forElems "table#field" $
    mappend P.clear $
      mconcat $ map (P.tr . mconcat . map showPoint) xss
    where
      showPoint mbcard = case mbcard of
        Nothing -> P.td ""
        Just card -> case card of
          (p, a)  -> (P.td $ show a) `P.attr` (P.atr "class" $ if p then "your-card" else "computers-card")

data Phase =
  Draw
  | Main
  | Move
    (Int, Int) -- subject of moving
  | Sacrifice
    Int -- rest cost of object of summon
  | Summon
    Int -- object of summon
  | End
  | Finish Bool

instance Show Phase where
  show p = case p of
    Draw          -> "ドロー"
    Main          -> "行動を選択"
    Move      _   -> "移動する位置を選択"
    Sacrifice cost-> "生贄を選択: エネルギーがあと" ++ (show $ cost) ++ "必要"
    Summon    _   -> "召喚する位置を選択"
    End           -> "手番を交代"
    Finish True   -> "あなたの勝ちです!"
    Finish False  -> "コンピュータが勝ち!"

data Game = Game {
  _players :: (Player, Player), -- (players ^. _1 . playerName) == "あなた"
  _areYourTurn :: Bool,
  _phase :: Phase,
  _field :: Field,
  _gen :: StdGen
  }
players :: Lens' Game (Player, Player); players = lens _players (\p x -> p { _players = x })
areYourTurn :: Lens' Game Bool; areYourTurn = lens _areYourTurn $ \p x -> p { _areYourTurn = x }
phase :: Lens' Game Phase; phase = lens _phase $ \p x -> p { _phase = x }
field :: Lens' Game Field; field = lens _field (\p x -> p { _field = x})
gen :: Lens' Game StdGen; gen = lens _gen $ \p x -> p { _gen = x }

turnPlayer :: Lens' Game Player
turnPlayer = lens getting setting
  where
    getting game = game ^. players . (if game ^. areYourTurn then _1 else _2)
    setting game p = players . (if game ^. areYourTurn then _1 else _2) .~ p $ game

sufficientForSummon :: Card -> [Card] -> Bool
sufficientForSummon card hand = (cost $ card) <= (foldl (+) (0 - energy card) $ map energy $ hand)

selectObjOfSummon :: Int -> Game -> Maybe Game
selectObjOfSummon i game = if sufficientForSummon (game ^. turnPlayer . hand . ix i) $ game^.turnPlayer.hand
  then Just $ phase .~ Summon i $ game
  else Nothing

delByIx :: Int -> [a] -> [a]
delByIx i xs = (take i xs) ++ (drop (i+1) xs)

selectSacrifice :: Int -> Int -> Game -> Game
selectSacrifice costOfObjOfSummon i game =
  if costOfObjOfSummon > (energy $ game ^. turnPlayer . hand . ix i)
     then game & phase .~ (Sacrifice $ costOfObjOfSummon - (energy $ game ^. turnPlayer . hand . ix i)) & addToDeck turnPlayer (game^.turnPlayer.hand.ix i) & turnPlayer . hand %~ delByIx i
     else game & addToDeck turnPlayer (game^.turnPlayer.hand.ix i) & turnPlayer . hand %~ delByIx i & phase .~ End

selectSbjOfMv :: Int -> Int -> Game -> Maybe Game
selectSbjOfMv i j game =
  case game ^. field . cell i j of
    Just c ->
      if ((game ^. areYourTurn) == (c ^. _1))
        then Just $ game & phase .~ Move (i, j)
        else Nothing
    Nothing -> Nothing

summonableZone :: Bool -> Lens' Field [Maybe (Bool, Card)]
summonableZone areYourTurn = lens ((if areYourTurn then last else head) . fromField) $ \(Field p) x -> Field $ if areYourTurn then (init p) ++ [x] else x:(tail p)

ix :: Int -> Lens' [a] a
ix i = lens (!! i) $ \p x -> (take i p) ++ [x] ++ (drop (i+1) p)

cell :: Int -> Int -> Lens' Field (Maybe (Bool, Card))
cell i j = (lens fromField (\(Field xss) yss -> Field yss)) . (ix i) . (ix j)

addToDeck :: Lens' Game Player -> Card -> Game -> Game
addToDeck pl card game =
  game & (pl . deck %~ ((card:) . (\x -> shuffle' x (length x) $ game ^. gen))) & gen %~ ((^. _2). (random::StdGen -> (Int, StdGen)))

move :: Int -> Int -> Int -> Int -> Game -> Maybe Game
move x y i j game =
  case game ^. field . cell x y of
    Just c ->
      if (i, j) `elem` (map ($ (x, y)) $ motionScope (game ^. areYourTurn) $ c ^. _2)
        then Just $ game
          & field . cell i j .~ (game ^. field . cell x y)
          & field . cell x y .~ Nothing
          & phase .~ End
        else Nothing
    Nothing -> Nothing

summon :: Int -> Int -> Card -> Game -> Maybe Game
summon objOfSummon for color game = let theHand = game ^. turnPlayer . hand in
  if isJust $ game ^. field . summonableZone (game ^. areYourTurn) . (ix for)
    then Nothing
    else Just $ game
      & turnPlayer . hand %~ delByIx objOfSummon
      & field . summonableZone (game ^. areYourTurn) . (ix for) .~ (Just $ (game^.areYourTurn, color))
      & (if 0 == (cost $ color)
        then phase .~ End
        else phase .~ Sacrifice (cost color))

draw :: Game -> Game
draw game = let get (a:newDeck) = Just a; get _ = Nothing in
  case get $ game ^. turnPlayer . deck of
    Just card -> game & turnPlayer . hand %~ (card:) & turnPlayer . deck %~ tail & phase .~ Main
    Nothing   -> game & phase .~ (Finish $ game^.areYourTurn)

operateWithHand :: Int -> Game -> Game
operateWithHand i game =
  case game ^. phase of
    Main ->
      case selectObjOfSummon i game of Just news -> news; Nothing -> game
    Sacrifice costOfObjOfSummon ->
      selectSacrifice costOfObjOfSummon i game
    Summon objOfSummon ->
      if i == objOfSummon
         then game & phase .~ Main
         else case selectObjOfSummon i game of Just news -> news; Nothing -> game
    _ -> game

operateWithField :: Int -> Int -> Game -> Game
operateWithField i j game =
  case game ^. phase of
    Main               -> fromMaybe game $ selectSbjOfMv i j game
    Move (x, y)        -> fromMaybe game $ move x y i j game
    Summon objOfSummon ->
      if (not (game ^. areYourTurn) || i == ((length $ fromField $ game ^. field) - 1)) && ((game ^. areYourTurn) || i == 0)
         then fromMaybe game $ summon objOfSummon j ((game ^. turnPlayer . hand) !! objOfSummon) game
         else game
    _                  -> game

instance P.ToElem Game where
  toElem game = mconcat [
    P.toElem $ game ^. field
   ,P.forElems "#status" $
      mappend P.clear $
        case game ^. phase of
          Finish _ -> P.toElem $ show $ game^.phase
          _        -> P.toElem $ "-- " ++ (game ^. turnPlayer & playerName) ++ "の番です、" ++ (show $ game ^. phase)
   ,game ^. players & both %~ P.toElem & uncurry mappend
   ,case game ^. phase of
      Main ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e $ "#"++(game^.turnPlayer & playerId) ++ " ol.hand li"
          let isSelectable card = sufficientForSummon card $ game^.turnPlayer.hand
          forM_ (zip handsLis $ map isSelectable $ game ^. turnPlayer . hand) $
            \(eachLi, isItSelectable) -> when isItSelectable $ setAttr eachLi "class" "selectable-hand"
          fieldTrs <- elemsByQS e "#field tr"
          fieldTdss <- mapM (`elemsByQS` "td") fieldTrs
          zipWithM_ (\el cl -> case cl of
              Just havitedCl -> if (havitedCl ^. _1) == (game ^. areYourTurn) then setAttr el "class" "movable-card" else return ()
              Nothing -> return ())
                (concat fieldTdss) $ concat $ fromField $ game ^. field
          return e
      Sacrifice costOfObjOfSummon ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e $ "#"++(game^.turnPlayer & playerId) ++ " ol.hand li"
          forM_ handsLis $ setAttr `flip` "class" `flip` "selectable-hand"
          return e
      Summon objOfSummon ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e $ "#"++(game^.turnPlayer & playerId) ++ " ol.hand li"
          setAttr (handsLis !! objOfSummon) "id" "obj-of-summon"
          fieldTrs <- elemsByQS e "#field tr"
          summonTds <- elemsByQS ((if game ^. areYourTurn then last else head) fieldTrs) "td"
          forM_ (zip summonTds (map isNothing $ game ^. field . (summonableZone $ game ^. areYourTurn))) $
            \(eachTd, isNotLived) -> when isNotLived $ setAttr eachTd "class" "summonable-zone"
          return e
      Move sbjOfMv ->
        P.Perch $ \e -> do
          fieldTrs <- elemsByQS e "#field tr"
          sbjTd <- fmap (!! (sbjOfMv ^. _2)) $ elemsByQS (fieldTrs !! (sbjOfMv^._1)) "td"
          setAttr sbjTd "id" "moving-subject"
          fieldTdss <- sequence $ map (elemsByQS `flip` "td") fieldTrs
          sequence_ $ map (setAttr `flip` "class" `flip` "motion-scope") $
            (map (\possibleMoving -> fieldTdss ^. ix (possibleMoving sbjOfMv ^. _1) . ix (possibleMoving sbjOfMv ^. _2)) $ motionScope (game ^. areYourTurn) $ view _2 $ fromJust $ game ^. field . uncurry cell sbjOfMv)
          return e
      _ ->
        mempty
    ]

initGame :: StdGen -> StdGen -> StdGen -> Game
initGame g h i =
  Game {
    _players =
      (initialDraw "あなた" "yours" show $ initDeck g,
        initialDraw "コンピュータ" "computers" (const "?") $ initDeck h)
    , _areYourTurn = True
    , _phase = Draw
    , _field = Field $ replicate 5 (replicate 3 Nothing)
    , _gen = i
  }
