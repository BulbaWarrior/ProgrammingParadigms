{-# LANGUAGE OverloadedStrings #-}

import CodeWorld



-- | A grid represented as a list of rows of "things".

data Grid a = Grid [[a]]



-- * A grid of pictures



-- | Picture of a wall.

wallPicture :: Picture

wallPicture = solidRectangle 0.95 0.95



-- | Picture of a floor.

floorPicture :: Picture

floorPicture = colored (light (light gray)) (solidRectangle 0.95 0.95)



-- | Doors will be differentiated by their index.

type DoorId = Int



-- | Doors and keys are distinguished visually using color.

doorIdColor :: DoorId -> Color

doorIdColor 0 = red

doorIdColor 1 = blue

doorIdColor 2 = green

doorIdColor n = light (doorIdColor (n - 3))



-- | Picture of a door with a given index.

doorPicture :: DoorId -> Picture

doorPicture doorId

  = colored (doorIdColor doorId) (solidCircle 0.3)

 <> wallPicture



-- | Picture of a key for a door with a given index.

keyPicture :: DoorId -> Picture

keyPicture doorId

  = scaled 0.5 0.5 (lettering "🔑")

 <> colored (doorIdColor doorId) (solidCircle 0.42)

 <> floorPicture



-- | Picture of a coin.

coinPicture :: Picture

coinPicture = scaled 0.7 0.7 (lettering "🍎") <> floorPicture



-- | A sample grid of pictures.

myPictureGrid :: Grid Picture

myPictureGrid = Grid

  [ [ w, w, w, w, w, w, w, w, w ]

  , [ w, c, w, f, f, f, w, f, w ]

  , [ w, f, w, f, w, f, f, f, w ]

  , [ w, f, f, f, w, w, w, f, w ]

  , [ w, f, w, f, w, f, f, f, w ]

  , [ w, f, w, w, w, w, d, w, w ]

  , [ w, f, w, c, w, f, f, f, w ]

  , [ w, k, w, f, f, f, w, c, w ]

  , [ w, w, w, w, w, w, w, w, w ]

  ]

  where

    w = wallPicture

    f = floorPicture

    k = keyPicture 1

    d = doorPicture 1

    c = coinPicture



-- | Exercise 6.1.

-- Implement this function. Try using higher-order functions.

renderRow :: [Picture] -> Picture

renderRow [] = blank

renderRow (p:rest) =

  p <> translated 1 0 (renderRow rest)



renderGrid :: Grid Picture -> Picture

renderGrid (Grid []) = blank

renderGrid (Grid (r:rest)) =

  renderRow r <> translated 0 (-1) (renderGrid (Grid rest))



-- * User-defined Tiles



-- | An item that can be placed on a floor tile.

data Item

  = Key DoorId  -- ^ A key for some door.

  | Coin        -- ^ A coin.



-- | A tile.

data Tile

  = Wall                -- ^ A wall tile.

  | Floor (Maybe Item)  -- ^ A floor tile, possibly with some item on it.

  | Door DoorId         -- ^ A door (with its index).



-- | A sample grid of Tiles.

myTileGrid :: Grid Tile

myTileGrid = Grid

  [ [ w, w, w, w, w, w, w, w, w ]

  , [ w, c, w, f, f, f, w, f, w ]

  , [ w, f, w, f, w, f, f, f, w ]

  , [ w, f, f, f, w, w, w, f, w ]

  , [ w, f, w, f, w, f, f, f, w ]

  , [ w, f, w, w, w, w, d, w, w ]

  , [ w, f, w, c, w, f, f, f, w ]

  , [ w, k, w, f, f, f, w, c, w ]

  , [ w, w, w, w, w, w, w, w, w ]

  ]

  where

    w = Wall

    f = Floor Nothing

    k = Floor (Just (Key 1))

    d = Door 1

    c = Floor (Just Coin)



-- | Exercise 6.2(a).

-- Implement this function.

renderItem :: Maybe Item -> Picture

renderItem Nothing = floorPicture

renderItem (Just (Key doorId)) =

  keyPicture doorId

renderItem (Just Coin) = coinPicture



-- | Exercise 6.2(b).

-- Implement this function.

renderTile :: Tile -> Picture

renderTile Wall = wallPicture

renderTile (Floor item) = renderItem item

renderTile (Door doorId) = doorPicture doorId



-- | Exercise 6.3.

-- Implement this function. Try using higher-order functions.



renderTileGrid :: Grid Tile -> Picture

renderTileGrid (Grid tileRows) =

  renderGrid (Grid (map (\row -> map renderTile row) tileRows))



-- | Exercise 6.4

removeItem :: Tile -> Tile

removeItem (Floor _) = Floor Nothing

removeItem x = x



removeItems :: Grid Tile -> Grid Tile

removeItems (Grid tileRows) =

  Grid (map (\row -> map removeItem row) tileRows)



-- | Exercise 6.5

mapGrid :: (a -> b) -> Grid a -> Grid b

mapGrid f (Grid rows) =

  Grid (map (\row -> map f row) rows)



-- | Exercise 6.6

openDoor :: Tile -> Tile

openDoor (Door _) = Floor Nothing

openDoor x = x

openDoors :: Grid Tile -> Grid Tile

openDoors grid = mapGrid openDoor grid



-- | Exercise 6.7

charToTile :: Char -> Tile

charToTile 'w' = Wall

charToTile 'f' = Floor Nothing

charToTile 'k' = Floor (Just (Key 1))

charToTile 'd' = Door 1

charToTile 'c' = Floor (Just Coin)



myTileGrid2 :: Grid Char -> Grid Tile

myTileGrid2 grid = mapGrid charToTile grid







main :: IO ()

main = drawingOf (scaled 2 2 (translated (-4) 4 (renderTileGrid (openDoors myTileGrid))))
