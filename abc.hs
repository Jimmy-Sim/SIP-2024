{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import Choreography
import Data.Proxy
import Data.Time
import System.Environment

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
  a `locally` \_ -> putStrLn "User A: Sending message to B and C"
  ab <- (a, aIntro) ~> b
  ac <- (a, aIntro) ~> c

  -- B sending to A and C:
  bIntro <- b `locally` \_ -> getLine
  b `locally` \_ -> putStrLn "User B: Sending message to A and C"
  ba <- (b, bIntro) ~> a
  bc <- (b, bIntro) ~> c

  -- C sending to A and B:
  cIntro <- c `locally` \_ -> getLine
  c `locally` \_ -> putStrLn "User C: Sending message to A and B"
  ca <- (c, cIntro) ~> a
  cb <- (c, cIntro) ~> b
  

  a `locally` \un -> putStrLn (un ba)
  a `locally` \un -> putStrLn (un ca)

  b `locally` \un -> putStrLn (un ab)
  b `locally` \un -> putStrLn (un cb)

  c `locally` \un -> putStrLn (un ac)
  c `locally` \un -> putStrLn (un bc)

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
