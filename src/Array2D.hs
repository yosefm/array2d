
module Array2D (
  Extents (..), Coords, flatIndex, numElems, inBounds,
  Arr2D, mkArr2D, nullArr2D, 
  (@), asRows, 
  merge)
  where 
 
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

data Arr2D a = Arr2D  Extents [a]  deriving Show

mkArr2D :: Extents -> [a] -> Maybe (Arr2D a)
mkArr2D e@(Ex rows cols) lst  
  | length lst == rows * cols  = Just $ Arr2D e lst
  | otherwise                  = Nothing

  
nullArr2D :: Arr2D a
nullArr2D = Arr2D (Ex 0 0) []

flatIndex :: Extents -> Coords -> Int
flatIndex (Ex _ cols) (row, col) = row*cols + col

(@) :: Arr2D a -> Coords -> a
Arr2D e lst @ c = lst !! flatIndex e c

asRows :: Arr2D a -> [[a]]
asRows (Arr2D (Ex _ cols) lst) = go lst where 
  go [] = []
  go lst' =   
    let (row, rest) = splitAt cols lst'
    in row : go rest

-- assumes indices are within the length.
-- indices in replacement list start at 0.
flatMergeSorted :: [a] -> Int -> [(Int, a)] -> [a]
flatMergeSorted [] _ _ = []
flatMergeSorted lst _ [] = lst
flatMergeSorted lst count ((at, what) : rest) = 
  let (h, t) = splitAt (at - count) lst
  in h ++ (what : flatMergeSorted (tail t) (at + 1) rest)

type Repl a = (Int, a)

ensureUnique :: [Repl a] -> Maybe [Repl a]
ensureUnique lst = 
  let srt = sortBy (compare `on` fst) lst
      dummyElem = (-1, snd $ head srt)
      cmpList = zip srt (dummyElem : srt)
      maybeUniqueElem (n1, n2)
        | fst n1 == fst n2 = Nothing
        | otherwise = Just n1
    
  in traverse maybeUniqueElem cmpList 

type Repl2D a = (Coords, a)
toRepl1D :: Extents -> Repl2D a -> Repl a
toRepl1D e src = ((flatIndex e . fst) src, snd src)

merge :: Arr2D a -> [Repl2D a] -> Maybe (Arr2D a)
merge (Arr2D e lst) repls = 
  let flatReps = ensureUnique $ map (toRepl1D e) repls
  in (Arr2D e . flatMergeSorted lst 0) <$> flatReps
