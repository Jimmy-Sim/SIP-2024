{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import MyHasChor.Choreography.Location
import MyHasChor.Choreography.NetworkAsync
import MyHasChor.Choreography.NetworkAsync.Http
import MyHasChor.Choreography.ChoreoAsync
import Control.Concurrent.Async
import MyHasChor.Choreography.Flaqr
import System.Environment
import System.Timeout 
import Data.Proxy
import Control.Monad
import GHC.TypeLits
import Data.List hiding (compare)
import Data.Monoid (Last(getLast))
import GHC.Conc.IO (threadDelay)
import Prelude hiding (compare)

user :: Proxy "user"
user = Proxy

bank1 :: Proxy "bank1"
bank1 = Proxy

bank2 :: Proxy "bank2"
bank2 = Proxy

bank3 :: Proxy "bank3"
bank3 = Proxy

messageChoreography :: Choreo IO ()
messageChoreography = do
  bal1 <- bank1 `locally` \_ -> getLine
  bank1 `locally` \_ -> putStrLn "Sending balance to user"
  b1 <- (bank1, bal1) ~> user
  bank1 `locally` \_ -> do
    putStrLn "Enter to terminate"
    getLine

  bal2 <- bank2 `locally` \_ -> getLine
  bank2 `locally` \_ -> putStrLn "Sending balance to user"
  b2 <- (bank2, bal2) ~> user
  bank2 `locally` \_ -> do
    putStrLn "Enter to terminate"
    getLine

  bal3 <- bank3 `locally` \_ -> getLine
  bank3 `locally` \_ -> putStrLn "Sending balance to user"
  b3 <- (bank3, bal3) ~> user
  bank3 `locally` \_ -> do
    putStrLn "Enter to terminate"
    getLine
  
  comp1 <- user `locally` \un -> compare (un b1) (un b2)
  comp2 <- user `locally` \un -> compare (un b2) (un b3)
  comp3 <- user `locally` \un -> compare (un b1) (un b3)

  sel1 <- user `locally` \un -> select (un comp1)  (un comp2)
  sel2 <- user `locally` \un -> select (un comp3) (un sel1)

  user `locally` \un -> do
    finalResult <- wait (un sel2)
    putStrLn $ "Balance: " ++ finalResult

  return ()


main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "user" -> runChoreography cfg messageChoreography "user"
    "bank1" -> runChoreography cfg messageChoreography "bank1"
    "bank2" -> runChoreography cfg messageChoreography "bank2"
    "bank3" -> runChoreography cfg messageChoreography "bank3"
  return ()
  where
    cfg = mkHttpConfig [ ("user",  ("localhost", 4242))
                       , ("bank1", ("localhost", 4343))
                       , ("bank2", ("localhost", 4444))
                       , ("bank3", ("localhost", 4545))
                       ]

