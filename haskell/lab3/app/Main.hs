module Main where

main :: IO ()
main = sumOfManyInputs
sumOfTwoInputs :: IO ()
sumOfTwoInputs = do
  putStrLn "Enter number"
  n1 <- getLine
  putStrLn "Enter another number"
  n2 <- getLine
  print ((read n1 :: Int) + (read n2 :: Int))


addInput :: Int -> Int -> IO ()
addInput 0 total = print total
addInput n total = do
  num <- getLine
  addInput (n - 1) (total + read num :: Int)

sumOfManyInputs :: IO ()
sumOfManyInputs = do
  n <- getLine
  addInput (read n :: Int) 0


data Question = Question String
data Answer = Answer (Maybe Int)
runQuestion :: Question -> IO Answer
runQuestion (Question question) = do
  putStrLn question
  str <- getLine
  return (Answer (Just (read str :: Int)))
