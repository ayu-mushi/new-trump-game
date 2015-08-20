{-# LANGUAGE Rank2Types #-}
module Game.BoardTrump.GameState
  (initGame, Game, Phase(..), selectSbjOfMv, phase, players, draw, summon, move, selectSacrifice, selectObjOfSummon, operateWithHand, operateWithField, isYourTurn, turnPlayer, field, cell, movableZone, isSummonable, summonableZone, gen) where
import Data.Monoid (mconcat, mempty, (<>), mappend)
import Data.List (insert)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr, setClass)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2, both)
import System.Random.Shuffle (shuffle')
import System.Random (StdGen, Random(random))
import Control.Monad (forM_, when, zipWithM_)
import Data.Maybe (isNothing, fromMaybe, isJust, fromJust)

import Game.BoardTrump.Cards
import Game.BoardTrump.Player
import Game.BoardTrump.Util

fieldToElem :: [[Maybe (Bool, Card)]] -> P.Perch
fieldToElem xss = P.forElems "table#field" $
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
  | Wait
  | Sacrifice
    Int -- rest cost of object of summon
  | Summon
    Int -- object of summon
  | End
  | Finish Bool deriving Eq

instance Show Phase where
  show p = case p of
    Draw          -> "ドロー"
    Main          -> "行動を選択"
    Wait          -> "パスします"
    Move      _   -> "移動する位置を選択"
    Sacrifice cost-> "生贄を選択: エネルギーがあと" ++ (show $ cost) ++ "必要"
    Summon    _   -> "召喚する位置を選択"
    End           -> "手番を交代"
    Finish True   -> "あなたの勝ちです!"
    Finish False  -> "コンピュータが勝ち!"

data Game = Game {
  _players :: (Player, Player), -- (players ^. _1 . playerName) == "あなた"
  _isYourTurn :: Bool,
  _phase :: Phase,
  _field :: [[Maybe (Bool, Card)]],
  _gen :: StdGen
  }
players :: Lens' Game (Player, Player); players = lens _players (\p x -> p { _players = x })
isYourTurn :: Lens' Game Bool; isYourTurn = lens _isYourTurn $ \p x -> p { _isYourTurn = x }
phase :: Lens' Game Phase; phase = lens _phase $ \p x -> p { _phase = x }
field :: Lens' Game [[Maybe (Bool, Card)]]; field = lens _field (\p x -> p { _field = x})
gen :: Lens' Game StdGen; gen = lens _gen $ \p x -> p { _gen = x }

turnPlayer :: Lens' Game Player
turnPlayer = lens getting setting
  where
    getting game = game ^. players . (if game ^. isYourTurn then _1 else _2)
    setting game p = players . (if game ^. isYourTurn then _1 else _2) .~ p $ game

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

-- TODO: 盤外に移動可能な場合でも選択肢としない
movableZone :: Card -> Game -> Int -> Int -> [(Int, Int)]
movableZone c game i j = filter (uncurry (uncurry (isMovable game) (i, j))) $ map ($ (i, j)) $ motionScope (game ^. isYourTurn) c

selectSbjOfMv :: Int -> Int -> Game -> Maybe Game
selectSbjOfMv i j game =
  case game ^. field . cell i j of
    Just c ->
      if ((game ^. isYourTurn) == (c ^. _1))
         && (not $ null $ movableZone (c^._2) game i j)
        then Just $ game & phase .~ Move (i, j)
        else Nothing
    Nothing -> Nothing

summonableZone :: Bool -> Lens' [[Maybe (Bool, Card)]] [Maybe (Bool, Card)]
summonableZone isYourTurn = lens (if isYourTurn then last else head) $ \p x -> if isYourTurn then (init p) ++ [x] else x:(tail p)

cell :: Int -> Int -> Lens' [[Maybe (Bool, Card)]] (Maybe (Bool, Card))
cell i j = (ix i) . (ix j)

addToDeck :: Lens' Game Player -> Card -> Game -> Game
addToDeck pl card game =
  game & (pl . deck %~ ((card:) . (\x -> shuffle' x (length x) $ game ^. gen))) & gen %~ ((^. _2). (random::StdGen -> (Int, StdGen)))

justMove :: Int -> Int -> Int -> Int -> Game -> Maybe Game
justMove srcX srcY tarX tarY game =
  case game ^. field . cell srcX srcY of
    Just from -> Just $ game
      & field . cell tarX tarY .~ Just from
      & field . cell srcX srcY .~ Nothing
      & if ((not $ game ^. isYourTurn) && (tarX+1) == (length $ game ^. field))
        || ((game ^. isYourTurn) && tarX == 0) then phase .~ Finish (game ^. isYourTurn) else phase .~ End
    Nothing -> Nothing

isMovable :: Game -> Int -> Int -> Int -> Int -> Bool
isMovable game srcX srcY tarX tarY =
  case game ^. field . cell srcX srcY of
    Just from ->
      if (tarX, tarY) `elem` (map ($ (srcX, srcY)) $ motionScope (game ^. isYourTurn) $ from ^. _2)
        then case game ^. field . cell tarX tarY of
          Just to -> if (from ^. _2) > (to ^. _2) && (to ^. _1) /= (from ^. _1)
            then True
            else False
          Nothing -> True
        else False
    Nothing -> False

move :: Int -> Int -> Int -> Int -> Game -> Maybe Game
move srcX srcY tarX tarY game = if isMovable game srcX srcY tarX tarY then justMove srcX srcY tarX tarY game else Nothing

summon :: Int -> Int -> Card -> Game -> Maybe Game
summon objOfSummon for color game = let theHand = game ^. turnPlayer . hand in
  if isJust $ game ^. field . summonableZone (game ^. isYourTurn) . (ix for)
    then Nothing
    else Just $ game
      & turnPlayer . hand %~ delByIx objOfSummon
      & field . summonableZone (game ^. isYourTurn) . (ix for) .~ (Just $ (game^.isYourTurn, color))
      & (if 0 == (cost $ color)
        then phase .~ End
        else phase .~ Sacrifice (cost color))

draw :: Game -> Game
draw game = let get (a:newDeck) = Just a; get _ = Nothing in
  case get $ game ^. turnPlayer . deck of
    Just card -> game & turnPlayer . hand %~ (card:) & turnPlayer . deck %~ tail & phase .~ Main
    Nothing   -> game & phase .~ (Finish $ not $ game^.isYourTurn)

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
    Move (x, y)        -> if x == i && y == j then game & phase .~ Main else fromMaybe game $ move x y i j game
    Summon objOfSummon ->
      if (not (game ^. isYourTurn) || i == ((length $ game ^. field) - 1)) && ((game ^. isYourTurn) || i == 0)
         then fromMaybe game $ summon objOfSummon j ((game ^. turnPlayer . hand) !! objOfSummon) game
         else game
    _                  -> game

isSummonable :: Game -> Card -> Bool
isSummonable game card = sufficientForSummon card $ game^.turnPlayer.hand

instance P.ToElem Game where
  toElem game = mconcat [
    fieldToElem $ game ^. field
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
          forM_ (zip handsLis $ map (isSummonable game) $ game ^. turnPlayer . hand) $
            \(eachLi, isItSelectable) -> when isItSelectable $ setClass eachLi "selectable-hand" True
          fieldTrs <- elemsByQS e "#field tr"
          fieldTdss <- mapM (`elemsByQS` "td") fieldTrs
          zipWithM_ (\el cl -> case cl of
              Just havitedCl ->
                do
                  i <- indexOfParentEl el
                  j <- indexEl 0 el
                  if havitedCl ^. _1 == game ^. isYourTurn && (not $ null $ movableZone (havitedCl^._2) game i j) then setClass el "movable-card" True else return ()
              Nothing -> return ())
                (concat fieldTdss)
                $ concat $ game ^. field
          return e
      Sacrifice costOfObjOfSummon ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e $ "#"++(game^.turnPlayer & playerId) ++ " ol.hand li"
          forM_ handsLis $ setClass `flip` "selectable-hand" `flip` True
          return e
      Summon objOfSummon ->
        P.Perch $ \e -> do
          handsLis <- elemsByQS e $ "#"++(game^.turnPlayer & playerId) ++ " ol.hand li"
          setAttr (handsLis !! objOfSummon) "id" "obj-of-summon"
          fieldTrs <- elemsByQS e "#field tr"
          summonTds <- elemsByQS ((if game ^. isYourTurn then last else head) fieldTrs) "td"
          forM_ (zip summonTds (map isNothing $ game ^. field . (summonableZone $ game ^. isYourTurn))) $
            \(eachTd, isNotLived) -> when isNotLived $ setAttr eachTd "class" "summonable-zone"
          return e
      Move sbjOfMv ->
        P.Perch $ \e -> do
          fieldTrs <- elemsByQS e "#field tr"
          sbjTd <- fmap (!! (sbjOfMv ^. _2)) $ elemsByQS (fieldTrs !! (sbjOfMv^._1)) "td"
          setAttr sbjTd "id" "moving-subject"
          fieldTdss <- mapM (elemsByQS `flip` "td") fieldTrs
          mapM_ (setClass `flip` "motion-scope" `flip` True) $
            (map (\possibleMoving -> fieldTdss ^. ix (possibleMoving sbjOfMv ^. _1) . ix (possibleMoving sbjOfMv ^. _2))
              $ filter (uncurry (uncurry (isMovable game) sbjOfMv) . ($ sbjOfMv))
                $ motionScope (game ^. isYourTurn) $ view _2 $ fromJust $ game ^. field . uncurry cell sbjOfMv)
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
    , _isYourTurn = True
    , _phase = Draw
    , _field = replicate 5 (replicate 3 Nothing)
    , _gen = i
  }
