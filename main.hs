module Main (main) where
import Haste (alert, Elem, toJSString, Event(OnClick), evtName, setTimeout, setCallback)
import Haste.DOM (elemsByQS)
import Haste.Foreign (ffi)
import Data.IORef
import qualified Haste.Perch as P
import Control.Monad (void, when)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Data.List (foldr1)
import Lens.Family2
import Lens.Family2.Stock
import System.Random (mkStdGen)

import Game.BoardTrump.GameState
import Game.BoardTrump.Player
import Game.BoardTrump.Cards
import Game.BoardTrump.Util
import Game.BoardTrump.CPU

refresh :: IORef Game -> IO ()
refresh reftoGame = void $ do
  game <- readIORef reftoGame
  body <- P.getBody
  P.build (P.toElem game) body

withTime :: IO () -> IO ()
withTime = setTimeout 1000

appendActWithTime :: IO () -> IO () -> IO ()
appendActWithTime a b = a >> withTime b

concatActWithTime :: [IO ()] -> IO ()
concatActWithTime = foldr1 appendActWithTime

turnChange :: IORef Bool -> IORef Game -> IO ()
turnChange isBusy reftoGame = do
  p <- readIORef isBusy
  when (not p) $
    concatActWithTime [
      writeIORef isBusy True,
      (modifyIORef reftoGame $ (phase .~ Draw) . (isYourTurn %~ not)) >> refresh reftoGame,
      (modifyIORef reftoGame draw) >> refresh reftoGame,
      do
        game <- readIORef reftoGame
        case game ^. isYourTurn of
             True -> return ()
             False -> runCPU isBusy reftoGame
        writeIORef isBusy False
      ]

runCPU :: IORef Bool -> IORef Game -> IO ()
runCPU isBusy reftoGame = do
  game <- readIORef reftoGame
  when (not $ game ^. isYourTurn) $ do
    let (play, g) = randomly game
    runPlayIO play isBusy reftoGame
    modifyIORef reftoGame $ gen .~ g
    withTime $ runCPU isBusy reftoGame

runPlayIO :: Play -> IORef Bool -> IORef Game -> IO ()
runPlayIO play isBusy reftoGame = do
  modifyIORef reftoGame $ runPlay play
  refresh reftoGame
  game <- readIORef reftoGame
  case game ^. phase of
       End  -> turnChange isBusy reftoGame
       Wait -> do
         withTime $ do
           modifyIORef reftoGame $ phase .~ End
           refresh reftoGame
           withTime $ turnChange isBusy reftoGame
       _    -> return ()

whenClickField :: IORef Bool -> IORef Game -> P.Perch
whenClickField isBusy reftoGame = P.forElems "#field" $ forIndexOfClickedTdElem $ \i j -> do
  modifyIORef reftoGame $ runPlay $ WithField (i, j)
  refresh reftoGame
  game <- readIORef reftoGame
  case game ^. phase of
    End -> turnChange isBusy reftoGame
    _   -> return ()
  return ()

whenClickHand :: IORef Bool -> IORef Game -> P.Perch
whenClickHand isBusy reftoGame = P.forElems "#yours ol.hand" $ forIndexOfClickedLiElem $ \i -> do
  game <- readIORef reftoGame
  when (game ^. isYourTurn) $ do
    modifyIORef reftoGame $ runPlay $ WithHand i
    refresh reftoGame
    case game ^. phase of
         End -> turnChange isBusy reftoGame
         _   -> return ()
  return ()

passButton :: IORef Bool -> IORef Game -> P.Perch
passButton isBusy reftoGame = P.forElems "button#pass" $ P.Perch $ \e -> do
  setCallback e OnClick $ \_ _ -> do
    game <- readIORef reftoGame
    when (game ^. isYourTurn) $ runPlayIO Pass isBusy reftoGame
  return e

main :: IO ()
main = do
  game <- initGame <$> newStdGen <*> newStdGen <*> newStdGen
  reftoGame <- newIORef game
  isBusy <- newIORef False
  P.getBody >>= P.build (passButton isBusy reftoGame <> whenClickHand isBusy reftoGame <> whenClickField isBusy reftoGame <> P.toElem game)
  withTime $ modifyIORef reftoGame draw >> refresh reftoGame

  where
    newStdGen = fmap mkStdGen $ ffi $ toJSString "(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })"
