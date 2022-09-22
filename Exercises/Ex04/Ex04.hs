{-# LANGUAGE FlexibleContexts #-}
module Ex04 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck
import Data.List
import Test.QuickCheck

capitalise :: FilePath -> FilePath -> IO ()
capitalise i o = do
  in_ <- openFile i ReadMode
  out <- openFile o WriteMode
  contents <- hGetContents in_
  hPutStr out (map toUpper contents)


sumFile :: IO ()
sumFile = do
  args <- getArgs
  let i = head args
  let o = head $ tail args
  contents <- readFile i
  putStr contents
  let nums = map (read::String->Int) (splitS contents [])
  writeFile o (show $ sum nums)


splitS :: String -> String -> [String]
splitS [] "" = []
splitS [] acc = [acc]
splitS (x:xs) acc
  | x == '\n' = [acc] ++ splitS xs []
  | otherwise = splitS xs (acc ++ [x])







data Player m = Player { guess :: m Int
                       , wrong :: Answer -> m ()
                       }
data Answer = Lower | Higher

guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
  where
   go 0 = pure False
   go n = do
     x' <- guess p
     case compare x x' of
       LT -> wrong p Lower  >> go (n-1)
       GT -> wrong p Higher >> go (n-1)
       EQ -> pure True

human :: Player IO
human = Player { guess = guess, wrong = wrong }
  where
    guess = do
      putStrLn "Enter a number (1-100):"
      x <- getLine
      case readMaybe x of
        Nothing -> guess
        Just i  -> pure i

    wrong Lower  = putStrLn "Lower!"
    wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1,100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")


midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo


ai :: Player (State (Int,Int))
ai = Player { guess = guess, wrong = wrong }
  where
    guess = do
      (lo,hi) <- get
      pure $ midpoint lo hi
    -- guess = uncurry midpoint <$> get
    -- guess = get >>= (pure . uncurry midpoint)
    -- guess = get >>= (\(lo, hi) -> pure $ midpoint lo hi)

    wrong Lower  = do
      (lo,hi) <- get
      if (hi - lo) > 2 then
        put $ (lo, midpoint lo hi)
      else
        put (lo, lo)

    wrong Higher = do
      (lo,hi) <- get
      if (hi - lo) > 2 then
        put $ (midpoint lo hi,hi)
      else
        put (hi, hi)

    


prop_basic (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x n ai) (1,n)

prop_optimality (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x (bound n) ai) (1,n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1


