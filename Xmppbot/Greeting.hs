module Xmppbot.Greeting where

import Control.Monad (liftM)
import System.Random (randomRIO)

-- Taken from <http://rosettacode.org/wiki/Pick_random_element#Haskell>
pick :: [a] -> IO a
pick xs = liftM (xs !!) $ randomRIO (0, length xs - 1)
