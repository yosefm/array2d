
module Array2D (
  Extents (..), Coords, flatIndex, fatIndex, numElems, inBounds,
  Arr2D, mkArr2D, nullArr2D, 
  (@), asRows, 
  merge)
  where 

import qualified Data.Vector as V
import Data.Vector ((//), (!))

import Data.List (sortBy)
import Data.Function (on)

data Extents = Ex Int Int deriving (Eq, Show)

numElems :: Extents -> Int
numElems (Ex rows cols) = rows*cols

inBounds :: Extents -> Coords -> Bool
inBounds (Ex rows cols) = (&&) 
  <$> ((&&) <$> (>= 0) <*> (< rows)) . fst 
  <*> ((&&) <$> (>= 0) <*> (< cols)) . snd

type Coords = (Int, Int)  -- row, col

data Arr2D a = Arr2D  Extents (V.Vector a)  deriving Show

mkArr2D :: Extents -> [a] -> Maybe (Arr2D a)
mkArr2D e@(Ex rows cols) lst  
  | length lst == rows * cols  = Just $ Arr2D e $ V.fromList lst
  | otherwise                  = Nothing

  
nullArr2D :: Arr2D a
nullArr2D = Arr2D (Ex 0 0) V.empty

flatIndex :: Extents -> Coords -> Int
flatIndex (Ex _ cols) (row, col) = row*cols + col

fatIndex :: Extents -> Int -> Coords
fatIndex (Ex _ cols) flat = divMod flat cols

(@) :: Arr2D a -> Coords -> a
Arr2D e v @ c = v ! flatIndex e c

asRows :: Arr2D a -> [V.Vector a]
asRows (Arr2D e@(Ex rows cols) v) = map (\n -> getRow n) [0..rows - 1]
  where getRow n = V.slice (flatIndex e (n,0)) cols v

type Repl a = (Int, a)
type Repl2D a = (Coords, a)

toRepl1D :: Extents -> Repl2D a -> Repl a
toRepl1D e src = ((flatIndex e . fst) src, snd src)

merge :: Arr2D a -> [Repl2D a] -> Maybe (Arr2D a)
merge (Arr2D e v) repls = 
  let flatReps = map (toRepl1D e) repls
  in Just $ Arr2D e $ v // flatReps
