{-# LANGUAGE Rank2Types, PackageImports #-}
module Game.BoardTrump.Core
  (initGame, Game, Phase(..), selectSbjOfMv, phase, players, draw, summon, move, selectSacrifice, selectObjOfSummon, isYourTurn, turnPlayer, field, cell, movableZone, isSummonable, summonableZone, gen, Play(..), runPlay) where
import Data.Monoid (mconcat, mempty, (<>), mappend)
import Data.List (insert)
import qualified Haste.Perch as P
import Haste (alert)
import Haste.DOM (elemsByQS, setAttr, setClass)
import Lens.Family2
import Lens.Family2.Unchecked
import Lens.Family2.Stock (_1, _2, both)
import Lens.Family2.State
import System.Random.Shuffle (shuffle')
import System.Random (StdGen, Random(random))
import Control.Monad (forM_, when, zipWithM_)
import Data.Maybe (isNothing, fromMaybe, isJust, fromJust)
import "mtl" Control.Monad.State.Strict

import Game.BoardTrump.Types
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

selectObjOfSummon :: Int -> Game -> Game
selectObjOfSummon i game = if sufficientForSummon (game ^. turnPlayer . hand . ix i) $ game^.turnPlayer.hand
  then phase .~ Summon i $ game
  else game

delByIx :: Int -> [a] -> [a]
delByIx i xs = (take i xs) ++ (drop (i+1) xs)

selectSacrifice :: Int -> Int -> Game -> Game
selectSacrifice costOfObjOfSummon i game = (`execState` game) $ do
  selected <- use $ turnPlayer . hand . ix i
  modify $ addToDeck turnPlayer selected
  turnPlayer . hand %= delByIx i
  if costOfObjOfSummon > energy selected
     then phase .= (Sacrifice $ costOfObjOfSummon - (energy selected))
     else phase .= End

movableZone :: Card -> Game -> Int -> Int -> [(Int, Int)]
movableZone c game i j = filter (uncurry (uncurry (isMovable game) (i, j))) $ map ($ (i, j)) $ motionScope (game ^. isYourTurn) c

selectSbjOfMv :: Int -> Int -> Game -> Game
selectSbjOfMv i j game =
  case game ^. field . cell i j of
    Just c ->
      if ((game ^. isYourTurn) == (c ^. _1))
         && (not $ null $ movableZone (c^._2) game i j)
        then game & phase .~ Move (i, j)
        else game
    Nothing -> game

summonableZone :: Bool -> Lens' [[Maybe (Bool, Card)]] [Maybe (Bool, Card)]
summonableZone isYourTurn = lens (if isYourTurn then last else head) $ \p x -> if isYourTurn then (init p) ++ [x] else x:(tail p)

cell :: Int -> Int -> Lens' [[Maybe (Bool, Card)]] (Maybe (Bool, Card))
cell i j = (ix i) . (ix j)

addToDeck :: Lens' Game Player -> Card -> Game -> Game
addToDeck pl card game =
  game & (pl . deck %~ ((card:) . (\x -> shuffle' x (length x) $ game ^. gen))) & gen %~ ((^. _2). (random::StdGen -> (Int, StdGen)))

isInField :: Game -> (Int, Int) -> Bool
isInField game (i, j) = i >= 0 && j >= 0 && i < (length (game ^. field)) && j < length (head (game ^. field))

isMovable :: Game -> Int -> Int -> Int -> Int -> Bool
isMovable game srcX srcY tarX tarY =
  case game ^. field . cell srcX srcY of
    Just from ->
      if isInField game (tarX, tarY) && (tarX, tarY) `elem` (map ($ (srcX, srcY)) $ motionScope (game ^. isYourTurn) $ from ^. _2)
        then case game ^. field . cell tarX tarY of
          Just to -> if battle (from ^. _2) (to ^. _2) && (to ^. _1) /= (from ^. _1)
            then True
            else False
          Nothing -> True
        else False
    Nothing -> False

move :: Int -> Int -> Int -> Int -> Game -> Game
move srcX srcY tarX tarY game =
  if isMovable game srcX srcY tarX tarY
     then case game ^. field . cell srcX srcY of
       Just from -> execState `flip` game $ do
         field . cell tarX tarY .= Just from
         field . cell srcX srcY .= Nothing
         p <- use isYourTurn
         if 1 == tarY && ((not p && (tarX+1) == (length $ game ^. field)) || (p && tarX == 0))
           then phase .= Finish p
           else phase .= End
       Nothing -> game
     else game

summon :: Int -> Int -> Card -> State Game ()
summon objOfSummon for card = do
  p <- use isYourTurn
  cl <- use $ field . summonableZone p . ix for
  if isJust cl
     then return ()
     else do
       turnPlayer . hand %= delByIx objOfSummon
       field . summonableZone p . ix for .= Just (p, card)
       if cost card == 0
          then phase .= End
          else phase .= Sacrifice (cost card)

draw :: Game -> Game
draw = execState $ do
  d <- use $ turnPlayer . deck
  case d of
       (card:d') -> do
         turnPlayer.hand %= (card:)
         turnPlayer.deck .= d'
         phase .= Main
       _ -> do
         p <- use isYourTurn
         phase .= (Finish $ not p)

data Play = WithHand Int | WithField (Int, Int) | Pass deriving Show

runPlay :: Play -> Game -> Game
runPlay play game = case play of
  WithHand i ->
    case game ^. phase of
         Main -> selectObjOfSummon i game
         Sacrifice costOfObjOfSummon -> selectSacrifice costOfObjOfSummon i game
         Summon objOfSummon ->
           if i == objOfSummon
              then game & phase .~ Main
              else selectObjOfSummon i game
         _ -> game
  WithField (i, j) ->
    case game ^. phase of
         Main               -> selectSbjOfMv i j game
         Move (x, y)        -> if x == i && y == j then game & phase .~ Main else move x y i j game
         Summon objOfSummon ->
           if (not (game ^. isYourTurn) || i == ((length $ game ^. field) - 1)) && ((game ^. isYourTurn) || i == 0)
              then execState (summon objOfSummon j ((game ^. turnPlayer . hand) !! objOfSummon)) game
              else game
         _                  -> game
  Pass -> case game ^. phase of
               Sacrifice _ -> game
               _           -> game & phase .~ Wait

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
          setAttr sbjTd "id" "subject-of-moving"
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
