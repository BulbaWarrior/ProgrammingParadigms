module Main where

k :: (t -> String) -> [t] -> [IO ()]
k g = map (\x -> putStrLn (g x))

data Result a = Success a | Failure String
divide :: Int -> Int -> Result Int
divide _ 0 = Failure "division by zero"
divide n m = Success (n `div` m)

splitResults :: [Result a] -> ([String], [a])
splitResults [] = ([], [])
splitResults (cur:rest) =
  case cur of
    Success s -> (failures, s:results)
    Failure f -> (f:failures, results)
  where
    (failures, results) = splitResults rest

data Grid a = Grid [[a]]

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f (Grid a) =
  Grid (map (\x -> map f x) a)


enumerateFrom :: Int -> [a] -> [(Int, a)]
enumrateFrom n [] = []
enumrateFrom n (a:rest) = (n, a):enumrateFrom (n+1) rest

enumerateGrid :: Grid a -> Grid (Int, a)
enumerateGrid Grid a = foldl (\state row -> )

main :: IO ()
main = putStrLn "Hello, Haskell!"
