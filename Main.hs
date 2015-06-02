import FFI
import Prelude

newtype Card = Card { getNumber :: Int }

data Player = Player {
  hand :: [Card]
  }

data Field = Field {
  
  }

data Game = Game {
  players :: (Player, Player)
  }

main :: Fay ()
main = alert "Hello, World!"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
