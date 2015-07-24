module Main (main) where
import Haste (alert, Elem, toJSString, Event(OnClick), evtName, setTimeout)
import Haste.DOM (elemsByQS)
import Haste.Foreign (ffi)
import Control.Concurrent
import qualified Haste.Perch as P
import Control.Monad (void, when)
import Data.Monoid ((<>))
import Data.List (foldr1)
import Lens.Family2
import Lens.Family2.Stock
import System.Random (mkStdGen)

import NewTrumpGame.GameState
import NewTrumpGame.Player
import NewTrumpGame.Cards
import NewTrumpGame.Util

refresh :: MVar Game -> IO ()
refresh reftoGame = void $
  withMVar reftoGame $ \game -> do
    body <- P.getBody
    P.build (P.toElem game) body

withTime :: IO () -> IO ()
withTime = setTimeout 1000

appendActWithTime :: IO () -> IO () -> IO ()
appendActWithTime a b = a >> withTime b

concatActWithTime :: [IO ()] -> IO ()
concatActWithTime = foldr1 appendActWithTime

turnChange :: MVar Game -> IO ()
turnChange reftoGame = concatActWithTime [
  return (),
  (modifyMVar_ reftoGame $ return . (phase .~ Draw) . (isYourTurn %~ not)) >> refresh reftoGame,
  (modifyMVar_ reftoGame $ return . draw) >> refresh reftoGame
  ]

whenClickField :: MVar Game -> P.Perch
whenClickField reftoGame = P.forElems "#field" $ forIndexOfClickedTdElem $ \i j -> do
  modifyMVar_ reftoGame $ return . operateWithField i j
  refresh reftoGame
  withMVar reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame; _ -> return ()
  return ()

whenClickHand :: MVar Game -> P.Perch
whenClickHand reftoGame = P.forElems "#yours ol.hand" $ forIndexOfClickedLiElem $ \i -> do
  modifyMVar_ reftoGame $ return . operateWithHand i
  refresh reftoGame
  withMVar reftoGame $ \game -> case game ^. phase of End -> turnChange reftoGame; _ -> return ()
  return ()

main :: IO ()
main = do
  g <- newStdGen
  h <- newStdGen
  i <- newStdGen
  let game = initGame g h i
  reftoGame <- newMVar game
  body <- P.getBody
  P.build (whenClickHand reftoGame <> whenClickField reftoGame <> P.toElem game) body
  withTime $ modifyMVar_ reftoGame (return . draw) >> refresh reftoGame

  where
    newStdGen = fmap mkStdGen $ ffi $ toJSString "(function(){ return Math.floor(Math.random() * Math.pow(2, 53)); })"
