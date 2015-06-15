module Html5
  (Html5Elem(..)) where

import Data.Maybe (isNothing, fromJust)
import Data.List (unwords)
import Haste.Perch ()

data Html5Elem = DivEl Div

instance Show Html5Elem where
  show (DivEl e) = show e

data Div = Div {
  cls :: [String],
  iden :: Maybe String,
  content :: Html5Elem
  }

instance Show Div where
  show e =
    "<div"
    ++ (if isNothing $ iden e then "" else " id="++ (fromJust $ iden e))
    ++ (if null $ cls e then "" else " class=" ++ (unwords $ cls e))
    ++ ">"
    ++ (show $ content e)
    ++ "</div>"
