module ArrayUtil
  ( createNewArray
  , createNewArrayFn
  , mapWithIndex
  , getElements
  , setElement
  ) where

import Data.Ix
import Data.Array.IArray

type Ix2 = (Int, Int)

createNewArray :: Int -> Int -> e -> Array Ix2 e
createNewArray w h e = array ((0,0), (w-1, h-1)) [((i, j), e) | i <- [0..w-1], j <- [0..h-1]]

createNewArrayFn :: (IArray a e) => Int -> Int -> ((Int, Int) -> e) -> a Ix2 e
createNewArrayFn w h f = array ((0,0), (w-1, h-1)) [((i, j), f (i,j)) | i <- [0..w-1], j <- [0..h-1]]

--createNewArrayFromLists :: Int -> Int -> [e] -> Array Ix2 e
--createNewArrayFromLists w h xs = array ((0,0), (w-1, h-1))

mapWithIndex :: (IArray a e) => (a Ix2 e -> Ix2 -> e -> e) -> a Ix2 e -> a Ix2 e
mapWithIndex f a = array (bounds a) [((x, y), f a (x, y) (elem (x,y))) | x <- [wa..wb], y <- [ha..hb]] 
  where (origin, end) = bounds a
        (wa, ha) = origin
        (wb, hb) = end
        elem i = (!) a i

getElements :: (IArray a e) => a Ix2 e -> [Ix2] -> [e]
getElements a is = map ((!) a) safeList
  where safeList = filter isSafe is
        isSafe (x,y) = wa <= x && x < wb && ha <= y && y < hb
        (origin, end) = bounds a
        (wa, ha) = origin
        (wb, hb) = end
        elem i = (!) a i

setElement :: (IArray a e) => a Ix2 e -> Ix2 -> e -> a Ix2 e
setElement array index element = createNewArrayFn (wb+1) (hb+1) setter
  where (origin, end) = bounds array
        (wa, ha) = origin
        (wb, hb) = end
        setter x = if x == index then element 
                   else array ! x
