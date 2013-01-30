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

not          = liftF $ Not () 
stringMode   = liftF $ TStringMode ()
trampoline   = liftF $ Trampoline ()
pop          = liftF $ Pop ()
remainder    = liftF $ Remainder ()
inputInt     = liftF $ InputInt ()
mult         = liftF $ Mult ()
add          = liftF $ Add ()
outChar      = liftF $ OutChar ()
divide       = liftF $ Div ()
number n     = liftF $ Num n ()
dup          = liftF $ Dup ()
goWest       = liftF $ GoWest ()
goEast       = liftF $ GoEast ()
goNorth      = liftF $ GoNorth ()
goSouth      = liftF $ GoSouth ()
goAway       = liftF $ GoAway ()
swap         = liftF $ Swap ()
ifEastWest   = liftF $ IfEastWest ()
greaterThan  = liftF $ GreaterThan ()
get          = liftF $ Get ()
put          = liftF $ Put ()
ifNorthSouth = liftF $ IfNorthSouth ()
inputChar    = liftF $ InputChar ()
stop         = liftF $ Stop ()

instructions = map 
  [('!',not)
  ,('"',stringMode)
  ,('#',trampoline)
  ,('$',pop)
  ,('%',remainder)
  ,('&',inputInt)
  ,('*',mult)
  ,('+',add)
  ,(',',outChar)
  ,('/',divider)
  ,(':',dup)
  ,('<',goWest)
  ,('>',goEast)
  ,('^',goNorth)
  ,('v',goSouth)
  ,('?',goAway)
  ,('\',swap)
  ,('_',ifEastWest)
  ,('`',greaterThan)
  ,('g',get)
  ,('p',put)
  ,('|',ifNorthSouth)
  ,('~',inputChar)
  ,('@',stop)]

   

-- parse = 

main = putStrLn "ok"
