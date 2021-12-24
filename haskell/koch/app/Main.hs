module Main where
import CodeWorld

koch :: Int -> Picture
koch 0 = polyline [(0, 0), (3, 0)]
koch n = segment <>
         translated 1 0 (rotated (pi / 3) segment) <>
         translated 1.5 ((sqrt 3) / 2) (rotated (-pi / 3) segment) <>
         translated 2 0 segment
  where
    segment = dilated (1/3) (koch (n - 1))

koch_star :: Int -> Picture
koch_star n = segment <>
              translated 3 0 (rotated (-2*pi/3) segment) <>
              translated (3*(1/2)) (-3*(sqrt 3/2)) (rotated (2*pi / 3) segment)
  where
    segment = koch n

main :: IO ()
main = drawingOf (dilated 2 (koch_star 5))
