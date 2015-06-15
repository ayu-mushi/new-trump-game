module StrictPerch (DomObj, indexEl, document, body) where

import Haste.Foreign (ffi)
import Haste (toJSString, Elem)
import qualified Haste.Perch as P (PerchM(..), Perch, getBody, getDocument)
import System.IO.Unsafe (unsafePerformIO)

prevNode :: P.Perch
prevNode = P.Perch $ ffi $ toJSString "function(node){ return node.previousSibling }"

isNull :: Elem -> Bool
isNull = unsafePerformIO . (ffi $ toJSString "(function(x) {return x === null;})")

newtype DomObj = DomObj { fromDomObj :: Elem }

-- ref: http://www.tagindex.com/kakolog/q4bbs/1701/1993.html
indexEl :: DomObj -> IO Int
indexEl = (indexEl' `flip` 0) . fromDomObj
  where
    indexEl' :: Elem -> Int -> IO Int 
    indexEl' tag i =
      if isNull tag
        then return i
        else do
          tag' <- P.build prevNode tag
          indexEl' tag' $ succ i

document :: IO DomObj
document = fmap DomObj P.getDocument

body :: IO DomObj
body = fmap DomObj P.getBody
