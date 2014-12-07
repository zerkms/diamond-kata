import Data.List (elemIndex)

letters = ['A'..'Z']

height :: Char -> Maybe Int
height c = elemIndex c letters

lpad :: Int -> String
lpad n = replicate n ' '

mirror :: [a] -> [a]
mirror xs = xs ++ (tail $ reverse xs)

line :: Int -> Int -> String
line h n = mirror (lpad (h - n) ++ [letters !! n] ++ lpad n)

top_part :: Int -> [String]
top_part h = [line h n ++ "\n" | n <- [0..h]]

generate :: Int -> String
generate = concat . mirror . top_part

diamond :: Char -> String
diamond c = case height c of
        Just h -> generate h
        _      -> "Not a valid input"

main = putStrLn $ diamond 'D'
