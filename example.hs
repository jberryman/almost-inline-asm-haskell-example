module Main where

import Data.Functor.Identity
import MyLib

main :: IO ()
main = do
  rnds <- readLn
  let t = runIdentity $ sipRounds 3 1 2 3 4
  if rnds == 3
    then do
      putStrLn "Since you said 3, let's check against expected value..."
      if (10679895449620852319,6850809759620305949,16840758994499871819,1302824994792767116) == t
        then putStrLn "Ok"
        else do 
          putStrLn "Uh oh not what we expected..."
          print t
    else print t

