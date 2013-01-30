{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Control.Monad.Free

data Instructions n r = 
      Not r                -- !
      | TStringMode r      -- "
      | Trampoline r       -- #
      | Pop r              -- $
      | Remainder r        -- %
      | InputInt r         -- &
      | Mult r             -- *
      | Add r              -- +
      | OutChar r          -- ,
      | Div r              -- /
      | Num n r            -- 0-9
      | Dup r              -- :
      | GoWest r           -- <
      | GoEast r           -- >
      | GoNorth r          -- ^
      | GoSouth r          -- v
      | GoAway r           -- ?
      | Swap r             -- \
      | IfEastWest r       -- _
      | GreaterThan r      -- `
      | Get r              -- g
      | Put r              -- p
      | IfNorthSouth r     -- |
      | InputChar r        -- ~
      | Stop r             -- @
      deriving (Show, Functor, Eq)

main = putStrLn "ok"
