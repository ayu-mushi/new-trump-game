import Data.Array (Array(Array))
import System.Random.Shuffle (shuffleM)
import Haste (mkCallback, alert, JSFun(JSFun), Elem, toJSString)
import Haste.Foreign (ffi, Unpacked)
import qualified Haste.Perch as P (PerchM(..), Perch)
import System.IO.Unsafe (unsafePerformIO)

import Cards

data Player = Player {
  hand :: [Card],
  deck :: [Card]
  }

newtype Field = Field { fromField :: Array (Int, Int) Card }

data Game = Game {
  players :: (Bool, Player, Player),
  field :: Field
  }

turnPlayer :: (Bool, Player, Player) -> Player
turnPlayer (p, a, b) = if p then a else b

prevNode :: P.Perch
prevNode = P.Perch $ ffi $ toJSString "function(node){ return node.previousSibling }"

isNull :: Elem -> Bool
isNull = unsafePerformIO . (ffi $ toJSString "(function(x) {return x === null;})")

indexEl :: Elem -> IO Int
indexEl = indexEl' `flip` 0
  where
    indexEl' :: Elem -> Int -> IO Int 
    indexEl' tag i =
      if isNull tag
        then return i
        else do
          tag' <- P.build prevNode tag
          indexEl' tag' $ succ i

foreign import ccall "setInterval" timeout :: JSFun (IO ()) -> Int -> IO ()

main :: IO ()
main = flip timeout 500 . mkCallback . alert $ "hello"
