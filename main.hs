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

withIORef :: IORef a -> (a -> IO b) -> IO b
withIORef ref act = readIORef ref >>= act

refresh :: IORef Game -> IO ()
refresh reftoGame = void $
  withIORef reftoGame $ \game -> do
    body <- P.getBody
    P.build (P.toElem game) body

withTime :: IO () -> IO ()
withTime = setTimeout 1000

appendActWithTime :: IO () -> IO () -> IO ()
appendActWithTime a b = a >> withTime b

concatActWithTime :: [IO ()] -> IO ()
concatActWithTime = foldr1 appendActWithTime

turnChange :: IORef Game -> IO () -> IO ()
turnChange reftoGame cont = concatActWithTime [
  return (),
  (modifyIORef reftoGame $ (phase .~ Draw) . (isYourTurn %~ not)) >> refresh reftoGame,
  (modifyIORef reftoGame draw) >> refresh reftoGame,
  cont
  ]

runCPU :: IORef Game -> IO ()
runCPU reftoGame = withIORef reftoGame $ \game ->
  if game ^. phase == End
    then turnChange reftoGame $ return ()
    else do
      let (play, g) = randomly game
      modifyIORef reftoGame $ \game -> game & runPlay play & gen .~ g
      refresh reftoGame
      withTime $ runCPU reftoGame

whenClickField :: IORef Game -> P.Perch
whenClickField reftoGame = P.forElems "#field" $ forIndexOfClickedTdElem $ \i j -> do
  modifyIORef reftoGame $ operateWithField i j
  refresh reftoGame
  withIORef reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame $ runCPU reftoGame; _ -> return ()
  return ()

whenClickHand :: IORef Game -> P.Perch
whenClickHand reftoGame = P.forElems "#yours ol.hand" $ forIndexOfClickedLiElem $ \i -> withIORef reftoGame $ \game -> if not $ game ^. isYourTurn then return () else do
  modifyIORef reftoGame $ operateWithHand i
  refresh reftoGame
  withIORef reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame $ runCPU reftoGame; _ -> return ()
  return ()

passButton :: IORef Game -> P.Perch
passButton reftoGame = P.forElems "button#pass" $ P.Perch $ \e -> do
  setCallback e OnClick $ \_ _ ->
    withIORef reftoGame $
      \game -> if game ^. isYourTurn then turnChange reftoGame $ runCPU reftoGame else return ()
  return e

main :: IO ()
main = do
  game <- initGame <$> newStdGen <*> newStdGen <*> newStdGen
  reftoGame <- newIORef game
  body <- P.getBody
  P.build (passButton reftoGame <> whenClickHand reftoGame <> whenClickField reftoGame <> P.toElem game) body
  withTime $ modifyIORef reftoGame draw >> refresh reftoGame

  where
    newStdGen = fmap mkStdGen $ ffi $ toJSString "(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })"
