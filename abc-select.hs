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

a :: Proxy "a"
a = Proxy

b :: Proxy "b"
b = Proxy

c :: Proxy "c"
c = Proxy

messageChoreography :: Choreo IO ()
messageChoreography = do
  -- A sending to B and C:
  aIntro <- a `locally` \_ -> getLine
  a `locally` \_ -> putStrLn "User A is sending message to C"
  ab <-  (a, aIntro) ~> c

  bIntro <- b `locally` \_ -> getLine
  b `locally` \_ -> putStrLn "User B is sending message to C"
  ab' <-  (b, bIntro) ~> c
  
  c `locally` \_ -> do putStrLn "C is waiting"
                       getLine
  ac <- c `locally` \un -> select (un ab) (un ab')
  c `locally` \un -> do
    f <- wait (un ac)
    putStrLn f
  a `locally` \_ -> do putStrLn "User A's message is sent"
                       getLine
  b `locally` \_ -> do putStrLn "User B's message is sent"
                       getLine
  return ()


main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "a" -> runChoreography cfg messageChoreography "a"
    "b" -> runChoreography cfg messageChoreography "b"
    "c" -> runChoreography cfg messageChoreography "c"
  return ()
  where
    cfg = mkHttpConfig [ ("a",  ("localhost", 4242))
                       , ("b", ("localhost", 4343))
                       , ("c", ("localhost", 4444))
                       ]
