{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE BangPatterns  #-}

module Main where

import Graphics.Gloss
import Data.Foldable (fold)
import Data.List (unfoldr)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Debug.Trace (trace)

window :: Display
window = InWindow "Ants the Movie" (800, 800) (10, 10)

data Grid a = Grid
  { width  :: a
  , height :: a
  , size   :: a
  } deriving Functor

renderGrid :: Grid Float -> Picture
renderGrid (Grid w h s) = translate (-h/2) (-w/2) $ vs <> hs
 where
  vs = fold [line [(0, s'), (h, s')] | s' <- [0,s..w]]
  hs = fold [line [(s', 0), (s', w)] | s' <- [0,s..h]]

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded)

data Turn = L | R

rotateEnum :: (Eq a, Enum a, Bounded a) => Turn -> a -> a
rotateEnum L c | c == minBound = maxBound
               | otherwise     = pred c
rotateEnum R c | c == maxBound = minBound
               | otherwise     = succ c

data Ant a = Ant Direction a a deriving Functor

renderAnt :: Float -> Ant Float -> Picture
renderAnt s (Ant _ x y) = translate (s*x - s/2) (s*y - s/2) $ color red $ rectangleSolid s s

turnAnt :: Turn -> Ant a -> Ant a
turnAnt t (Ant d x y) = Ant (rotateEnum t d) x y

forward :: Num a => Ant a -> Ant a
forward (Ant d x y) = Ant d x' y'
 where
  (x', y') = case d of
    North -> (x, y + 1)
    East  -> (x + 1, y)
    South -> (x, y - 1)
    West  -> (x - 1, y)

data Cell a = Cell Int a a deriving (Eq, Ord, Functor)

renderCell :: Float -> Cell Float -> Picture
renderCell s (Cell c x y) = translate (s*x - s/2) (s*y - s/2) $ color (colors !! c) $ rectangleSolid s s

data Langton = Langton
  { ant      :: Ant Int
  , rule     :: [Turn]
  , cells    :: Map (Int, Int) Int
  , nextCell :: Cell Int
  }

-- todo make a shitload more colors and stuff
colors :: [Color]
colors = cycle [magenta, black, greyN 0.5, green, cyan, blue, rose, violet, azure, aquamarine, chartreuse, orange, yellow, white]

initLangton :: [Turn] -> Langton
initLangton r = Langton (Ant North 0 0) r M.empty (Cell 0 0 0)

stepLangton :: Langton -> Langton
stepLangton (Langton ant@(Ant _ x y) r cs _) = Langton ma r uc (Cell nc x y)
 where
  ma    = forward $ turnAnt (r !! color) ant
  uc    = M.insert (x, y) nc cs
  color = maybe 0 id $ M.lookup (x, y) cs
  nc    = mod (color + 1) (length r)

renderLangton :: Float -> Langton -> Picture
renderLangton s (Langton ant _ cs _) = c <> a
 where
  -- TODO: Dynamic grid based on viewport
  -- g = renderGrid (Grid 800 800 s)
  a  = renderAnt s (fmap fromIntegral ant)
  c = M.foldMapWithKey (\(x, y) i -> renderCell s $ fmap fromIntegral (Cell i x y)) cs

renderNextCell :: Float -> Langton -> Picture
renderNextCell s (Langton ant _ _ nc) = renderAnt s (fromIntegral <$> ant) <> renderCell s (fromIntegral <$> nc)

unfoldLangton1 :: Float -> [Turn] -> [Picture]
unfoldLangton1 s r = unfoldr go (mempty, initLangton r)
 where
  go (p, l) = let p' = p <> renderNextCell s l
              in Just (p', (p', stepLangton l))

unfoldLangton2 :: [Turn] -> [Langton]
unfoldLangton2 r = unfoldr go (initLangton r)
 where
  go l = let l' = stepLangton l in Just (l', l')

simulated :: IO ()
simulated = simulate window white 10000 init render step
 where
  init     = initLangton [L,R,R,R,R,R,L,L,R]
  render   = renderLangton 5
  step _ _ = stepLangton

unfolded1 :: IO ()
unfolded1 = animate window white go
 where
  go t = steps !! (floor $ t * 200)
  steps = unfoldLangton1 5 [L,R,R,R,R,R,L,L,R]

unfolded2 :: IO ()
unfolded2 = animate window white go
 where
  go t = renderLangton 5 $ steps !! (floor $ t * 10000)
  steps = unfoldLangton2 [L,R,R,R,R,R,L,L,R]

main :: IO ()
main = simulated
