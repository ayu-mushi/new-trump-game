module Data.List.Zipper
  (Zipper) where

import Control.Lens

data Zipper a = Zip [a] [a]

empty :: Zipper a
empty = Zip [] []

fromList :: [a] -> Zipper a
fromList as = Zip [] as

toList :: Zipper a -> [a]
toList (Zip ls rs) = reverse ls ++ rs

focus :: Lens' (Zipper a) a
focus = lens (\(Zip _ (a:_)) -> a) $ \(Zip ls (_:rs)) a -> Zip ls (a:rs)
