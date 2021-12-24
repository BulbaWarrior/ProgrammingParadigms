module Main where

f :: ([a] -> [a]) -> a -> [a] -> [a]
f x y z = x (y : x z)

repeatUntil :: (a -> Bool) -> (a -> IO a) -> a -> IO a
repeatUntil p f x
  | p x = return x
  | otherwise = do
      x1 <- f x
      repeatUntil p f x1
main :: IO Int
main = repeatUntil (< 10) (\x -> return 5) 15


type Point = (Double, Double)
data Event = Click | Move Point

toPolylineHelper :: Point -> [Event] -> [Point]
toPolylineHelper _ [] = []
toPolylineHelper point (Click:evts) = (point:toPolylineHelper point evts)
toPolylineHelper _ (Move point1:evts) = toPolylineHelper point1 evts

toPolyline :: [Event] -> [Point]
toPolyline [] = []
toPolyline (Click:evts) = toPolyline evts
toPolyline (Move point:evts) = toPolylineHelper point evts
