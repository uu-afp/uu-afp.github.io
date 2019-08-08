module Main where

type Bounds = (Integer, Integer)
data Input = TooLow | TooHigh | Unknown
           deriving (Eq, Show)

next :: Bounds -> Integer
next (l, h) = (l + h) `div` 2

step :: Integer -> Bounds -> Input -> Bounds
step n (l, h) TooLow  = (n+1, h)
step n (l, h) TooHigh = (l, n-1)

key :: Char -> Input
key 'l' = TooLow
key 'L' = TooLow
key 'h' = TooHigh
key 'H' = TooHigh
key _   = Unknown

mIN_NUMBER, mAX_NUMBER :: Integer
mIN_NUMBER = 1
mAX_NUMBER = 100

main :: IO ()
main = do putStr "I am going to guess a number between "
          putStr (show mIN_NUMBER)
          putStr " and "
          putStr (show mAX_NUMBER)
          putStrLn " :)"
          go (mIN_NUMBER, mAX_NUMBER)
  where

    go :: Bounds -> IO ()
    go b = do let n = next b
              putStr "Is it "
              putStr (show n)
              putStr "? (y/n) "
              (c:_) <- getLine
              case c of
                'y' -> win
                'Y' -> win
                _   -> ask n b
   
    win :: IO ()
    win = putStrLn "Great! Bye!"

    ask :: Integer -> Bounds -> IO ()
    ask n b = do putStr "Is it too low or too high? (l/h) "
                 (c:_) <- getLine
                 case key c of
                   Unknown -> ask n b
                   result  -> go (step n b result)