
type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade
data Result a = Success a | Failure String deriving Show

studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA ((Student name A):rest) = name:(studentsWithA rest)
studentsWithA(_:rest) = studentsWithA rest

combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith _ (Failure str) _  = Failure str
combineResultsWith _ _ (Failure str) = Failure str
combineResultsWith f (Success a) (Success b) = Success (f a b)


main :: IO ()
main = print "Hello, world!"
