module Xmppbot.Greeting where

import System.Random (randomRIO)

-- Taken from <http://rosettacode.org/wiki/Pick_random_element#Haskell>
pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
