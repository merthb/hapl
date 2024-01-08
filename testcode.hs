f :: Int -> Int
f x = helper x 0 where
    helper :: Int -> Int -> Int
    helper a b = a + b

g :: Int -> Int
g x = helper 0 x where
    helper :: Int -> Int -> Int
    helper a b = a * b

h :: Int -> Int 
h x = helper x x where
    helper :: Int -> Int -> Int 
    helper a b = a - b

i :: Int -> Int
i x = helper 1 x where
    helper :: Int -> Int -> Int
    helper a b = a + b