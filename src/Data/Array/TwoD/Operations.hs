
-- {-# LANGUAGE FlexibleContexts #-}

module Data.Array.TwoD.Operations (
      prettyP
    , matrix0
    , slice
    , transpose
    , hflip
    , vflip
    , cw
    , ccw
  ) where

import Data.Array.IArray (Array, IArray, Ix, bounds, elems, ixmap, range, listArray, (!))
import Control.Monad (forM_)
import Data.Tuple (swap)

prettyP arr = do
  let (lo,hi) = bounds arr
      ys = [fst lo .. fst hi]
      xs = [snd lo .. snd hi]
  forM_ ys $ \y -> do 
    forM_ xs $ \x -> putStr $ show (arr ! (y,x)) ++ " "
    putStrLn ""


-- | ''matrix0 maxY maxX els'' will contruct a
-- | zero-based 2D matrix, with upper bounds (maxY, maxX),
-- from the elements ''els''.
matrix0
  :: (IArray a e, Ix t, Num t) =>
     t -> t -> [e] -> a (t, t) e
matrix0 maxY maxX = curry listArray (0, 0) (maxY,maxX)

-- | ''slice low high arr'' will return a slice of the array,
-- from bounds ''low'' to ''high''
slice
  :: (IArray a e, Ix t, Num t) =>
     (t, t) -> (t, t) -> a (t, t) e -> a (t, t) e
slice low high arr =
  let mapping (y, x) = (y + fst low, x + snd low)
  in  ixmap ((0,0), (fst high - fst low, snd high - snd low)) mapping arr

-- | transpose a 2D array
transpose
  :: (IArray a e, Ix t) => a (t, t) e -> a (t, t) e
transpose arr =
  let (low, high) = bounds arr
  in  ixmap (swap low, swap high) swap arr

-- | horizontal flip of a 2D array
hflip
  :: (IArray a e, Ix t, Num t) => a (t, t) e -> a (t, t) e
hflip arr =
  let (low, high) = bounds arr
      mapping (y, x) = (y, snd high - x)
  in  ixmap (low, high) mapping arr

-- | vertical flip of a 2D array
vflip
  :: (IArray a e, Ix t, Num t) => a (t, t) e -> a (t, t) e
vflip arr =
  let (low, high) = bounds arr
      mapping (y, x) = (fst high - y, x)
  in  ixmap (low, high) mapping arr


-- | rotate 2D array clockwise
-- (same as ''hflip . transpose'')
cw
  :: (IArray a e, Ix t, Num t) => a (t, t) e -> a (t, t) e
cw arr =
  let (low, high) = bounds arr
      mapping (y, x) = (fst high - x, y) 
  in ixmap (swap low, swap high) mapping arr

-- | rotate 2D array counter-clockwise
-- (same as ''vflip . transpose'')
ccw
  :: (IArray a e, Ix t, Num t) => a (t, t) e -> a (t, t) e
ccw arr =
  let (low, high) = bounds arr
      mapping (y, x) = (x, snd high - y) 
  in ixmap (swap low, swap high) mapping arr

