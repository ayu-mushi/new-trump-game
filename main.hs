import Data.Array (Array(Array))
import System.Random.Shuffle (shuffleM)
import Haste (mkCallback, alert, JSFun(JSFun), Elem, toJSString)
import Haste.Foreign (ffi)
import qualified Haste.Perch as P

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

indexEl :: P.Perch -> P.Perch -> IO (Maybe Int)
indexEl = undefined

foreign import ccall "setInterval" timeout :: JSFun (IO ()) -> Int -> IO ()

main :: IO ()
main = flip timeout 500 . mkCallback . alert $ "hello"
