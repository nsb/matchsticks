module Main where

type Name = String

class CanPlay a where
    turn :: a -> IO Int

data Player = Human Name
            | Computer
            deriving Show

instance CanPlay Player where
    turn (Human name) =
        do
            putStrLn "How many sticks?"
            choice <- getLine
            putStrLn (name ++ " takes " ++ show choice)
            return (read choice :: Int)
    turn Computer = do
        putStrLn "Computer takes 1..."
        return 1

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

game :: (CanPlay a) => Int -> [a] -> IO ()
game 0 _ =
    putStrLn "finished"
game matchsticks players =
    do
        print matchsticks
        number <- turn $ head players
        game (matchsticks - number) (rotate 1 players)

main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    game 3 [Human name, Computer]
