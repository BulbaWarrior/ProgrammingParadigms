module Main where
import CodeWorld

-- | A line with a focus.
-- Line xs y zs represents a descrete line:
-- * xs represents all elements to the left (below)
-- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a]
  deriving (Show) -- required to enable printing

integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- | Keep up to a given number of elements in each direction in a line.
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine n (Line left x right) = Line (take n left) x (take n right)

-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to
-- produce a list of elements to the right of x.

nums :: Int -> Maybe Int
nums x
  | x < 3 = Just (x + 1)
  | otherwise = Nothing

generate :: (a -> Maybe a) -> a -> [a]
generate f x =
  case (f x) of
    Just next -> (next:(generate f next))
    Nothing -> []

genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (generate f x) x (generate g x)

-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line left x right) = Line (map f left) (f x) (map f right)

-- | Zip together two lines.
-- zipLines integers integers
-- = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..]
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line left1 x1 right1) (Line left2 x2 right2) =
  Line (zip left1 left2) (x1, x2) (zip right1 right2)

-- | Zip together two lines with a given combining function.
-- zipLinesWith (*) integers integers
-- = Line [1,4,9,..] 0 [1,4,9,..]
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith f (Line l1 x1 r1) (Line l2 x2 r2) =
  Line (map func (zip l1 l2)) (f x1 x2) (map func (zip r1 r2))
  where
    func (a, b) = f a b

data Cell = Alive | Dead
  deriving (Show)

rule30 :: Line Cell -> Cell
rule30 (Line (left:_) x (right:_)) =
  case (left, x, right) of
    (Alive, Alive, Alive) -> Dead
    (Alive, Alive, Dead)  -> Dead
    (Alive, Dead, Alive)  -> Dead
    (Alive, Dead, Dead)   -> Alive
    (Dead, Alive, Alive)  -> Alive
    (Dead, Alive, Dead)   -> Alive
    (Dead, Dead, Alive)   -> Alive
    (Dead, Dead, Dead)    -> Dead

-- assume the non-existing neighbour is dead
rule30 (Line [] x (right:_)) =
  case (x, right) of
    (Dead, Dead) -> Dead
    (_, _)       -> Alive
rule30 (Line (left:_) x []) =
  case (left, x) of
    (Alive, Alive) -> Dead
    (Alive, Dead)  -> Alive
    (Dead, Alive)  -> Alive
    (Dead, Dead)   -> Dead

shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line [] _ _) = Nothing
shiftLeft (Line (next:left) x right) =
  Just (Line left next (x:right))

shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line _ _ []) = Nothing
shiftRight (Line left x (next:right)) =
  Just (Line (x:left) next right)

lineShifts :: Line a -> Line(Line a)
lineShifts line = genLine shiftLeft line shiftRight

applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)


applyOffsets :: Line Picture -> Line Picture
applyOffsets line =
  mapLine (\(picture, dx) -> translated (fromInteger dx) 0 picture) lineOffsets
  where
    lineOffsets = zipLines line integers
-- | Render a line of 1x1 pictures.(horizontally)
renderLine :: Line Picture -> Picture
renderLine line = renderLine_ (applyOffsets line)
renderLine_ :: Line Picture -> Picture
renderLine_ (Line left x right) = pictures left <> x <> pictures right

  -- mapLine (\(picture, dx) -> translated (fromInteger dx) 0 picture) lineOffsets
  -- where
  --   lineOffsets = zipLines line integers

renderCell :: Cell -> Picture
renderCell Alive = solidRectangle 1 1
renderCell Dead = rectangle 1 1

renderCells :: Line Cell -> Picture
renderCells line = renderLine (mapLine renderCell line)



-- | Render the fist N steps of Rule 30,
-- applied to a given starting line.
renderRule30 :: Int -> Line Cell -> Picture
renderRule30 0 line = renderCells line
renderRule30 n line = renderCells line <>
                      translated 0 (-1) (renderRule30 (n - 1) (applyRule30 line))

initialState :: Line Cell
initialState = genLine dead Alive dead
  where dead _ = Just Dead


data Space a = Space (Line (Line a)) deriving Show

extend :: Line a -> b -> Line (a, b)
extend line x =
  zipLines line xs
  where
    xs = genLine repeat x repeat
    repeat x = Just x


productOfLines :: Line a -> Line b -> Space (a, b)
productOfLines x (Line down y up) =
  Space (Line extendDown (extend x y) extendUp)
  where
    extendDown = map (extend x) down
    extendUp = map (extend x) up

mapSpace :: (a -> b) -> Space a -> Space b
mapSpace f (Space (Line left_lines line right_lines)) =
  Space (Line (map (mapLine f) left_lines) (mapLine f line) (map (mapLine f) right_lines))

zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space (Line l1 x1 r1)) (Space (Line l2 x2 r2)) =
  Space (Line (zipElems (zip l1 l2)) (zipLines x1 x2) (zipElems (zip r1 r2)))
  where
    zipElems :: [(Line a, Line b)] -> [Line (a, b)]
    zipElems = map (\(line1, line2) -> zipLines line1 line2)

-- | Zip together two spaces with a given combining function.
-- zipLinesWith (*) integers integers
-- = Line [1,4,9,..] 0 [1,4,9,..]
-- zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
-- zipSpacesWith
main :: IO ()
main = drawingOf (renderRule30 40 (cutLine 40 initialState))
