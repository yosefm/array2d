
module Array2D where 

data Extents = Ex Int Int deriving (Eq, Show)
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
