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
import Data.Maybe (fromJust)

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

data Turn = L | R | N | U deriving Show

rotateEnum :: (Eq a, Enum a, Bounded a) => Turn -> a -> a
rotateEnum L c | c == minBound = maxBound
               | otherwise     = pred c
rotateEnum R c | c == maxBound = minBound
               | otherwise     = succ c
rotateEnum N c = c
rotateEnum U c = rotateEnum L $ rotateEnum L c

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
  { ant   :: Ant Int
  , rule  :: [Turn]
  , cells :: Map (Int, Int) Int
  , steps :: Int
  }

-- todo make more colors and stuff
colors :: [Color]
colors = cycle [magenta, violet, greyN 0.5, green, cyan, blue, rose, black, azure, aquamarine, chartreuse, orange, yellow, white]

initLangton :: [Turn] -> Langton
initLangton r = Langton (Ant North 0 0) r M.empty 0

stepLangton :: Langton -> Langton
stepLangton (Langton ant@(Ant _ x y) r cs s) = Langton ma r uc (s + 1)
 where
  ma    = forward $ turnAnt (r !! color) ant
  uc    = M.insert (x, y) nc cs
  color = maybe 0 id $ M.lookup (x, y) cs
  nc    = mod (color + 1) (length r)

renderLangton :: Float -> Langton -> Picture
renderLangton s (Langton ant _ cs steps) = c <> a
 where
  -- TODO: Dynamic grid based on viewport
  -- g = renderGrid (Grid 800 800 s)
  a  = renderAnt s (fmap fromIntegral ant)
  c = M.foldMapWithKey (\(x, y) i -> renderCell s $ fmap fromIntegral (Cell i x y)) cs

parseRule :: String -> Maybe [Turn]
parseRule = traverse parseTurn
 where
  parseTurn c = case c of
    'L' -> Just L
    'R' -> Just R
    'N' -> Just N
    'U' -> Just U
    _   -> Nothing

simulated :: IO ()
simulated = simulate window (greyN 0.1) 100 init render step
 where
  init     = initLangton $ fromJust $ parseRule "RL"
  render   = renderLangton 1
  step _ _ = stepLangton

main :: IO ()
main = simulated
