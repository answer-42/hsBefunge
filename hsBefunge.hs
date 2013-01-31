{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad

import Prelude hiding (not)

import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr

{- TODO
  - Make a field that we can parse, we will use a Repa Array for that.
  -}

data Instructions n = 
      Not                -- !
      | TStringMode      -- "
      | Trampoline       -- #
      | Pop              -- $
      | Remainder        -- %
      | InputInt         -- &
      | Mult             -- *
      | Add              -- +
      | OutChar          -- ,
      | Div              -- /
      | Num n            -- 0-9
      | Dup              -- :
      | GoWest           -- <
      | GoEast           -- >
      | GoNorth          -- ^
      | GoSouth          -- v
      | GoAway           -- ?
      | Swap             -- \
      | IfEastWest       -- _
      | GreaterThan      -- `
      | Get              -- g
      | Put              -- p
      | IfNorthSouth     -- |
      | InputChar        -- ~
      | Stop             -- @
      deriving (Show, Functor, Eq)

instructions = 
  [('!',Not)
  ,('"',TStringMode)
  ,('#',Trampoline)
  ,('$',Pop)
  ,('%',Remainder)
  ,('&',InputInt)
  ,('*',Mult)
  ,('+',Add)
  ,(',',OutChar)
  ,('/',Div)
  ,(':',Dup)
  ,('<',GoWest)
  ,('>',GoEast)
  ,('^',GoNorth)
  ,('v',GoSouth)
  ,('?',GoAway)
  ,('\\',Swap)
  ,('_',IfEastWest)
  ,('`',GreaterThan)
  ,('g',Get)
  ,('p',Put)
  ,('|',IfNorthSouth)
  ,('~',InputChar)
  ,('@',Stop)]

main = putStrLn "ok"
