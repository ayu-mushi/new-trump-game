module Game.BoardTrump.CPU () where

newtype Play = Play { Either Int (Int, Int) }

advantage :: Game -> Int
advantage = undefined

possiblePlay :: Game -> [Play]
possiblePlay = undefined
