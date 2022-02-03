-- Define a function that joins a list of lists together using a separator value:

intersperse :: a -> [[a]] -> [a]
intersperse s xs = intersperseAcc [] xs
  where intersperseAcc acc [] = acc
        intersperseAcc acc xs | length xs == 1 = acc ++ (head xs)
        intersperseAcc acc (h:t) = intersperseAcc (acc ++ h ++ [s]) t
