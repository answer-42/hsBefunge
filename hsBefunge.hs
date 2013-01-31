{-# LANGUAGE TypeOperators #-}
module Main where

import System.Environment (getArgs)

import Control.Monad (mapM,liftM,liftM2)

import Data.Char (isAscii,isDigit,digitToInt)

import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Vector

{- TODO
  - Make a field that we can parse, we will use a Repa Array for that.
  -}

data Instructions n s = 
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
      | Character s      -- Characters in Stringmode
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
      | Empty            -- Just to make an Empty stack
      deriving (Show,Eq)

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
  ,('@',Stop)
  ,(' ',Empty)]

data Direction = North | East | South | West deriving (Eq)

data Program = Program {
   stack       :: Array V R.DIM1 (Instructions Int Char)
  ,direction   :: Direction  
  ,fungeSpace  :: Maybe (Array V R.DIM2 (Instructions Int Char))
  ,currentPos  :: R.DIM2
}

stackSize  = 1024 :: Int
befungeDim = (80,25) :: (Int,Int)

parse ::  String -> Maybe (Array V R.DIM2 (Instructions Int Char))
parse x = liftM (fromListVector (Z :. fst befungeDim :. snd befungeDim)) 
                           $ getIns False $ addEmptyRows $ foldr ((++) . fillUp) [] $ lines x
  where fillUp l = if length l < fst befungeDim
                   then l ++ replicate (fst befungeDim-length l) ' '
                   else l
        
        addEmptyRows x = if length x < (uncurry (*) befungeDim)
                         then x++replicate ((uncurry (*) befungeDim)-length x) ' '
                         else x

        getIns :: Bool -> String -> Maybe [Instructions Int Char ]
        getIns True (x:xs) | x == '"'  = liftM2 (:) (lookup x instructions) $ getIns False xs
                           | isAscii x = liftM2 (:) (Just $ Character x) $ getIns True xs
                           | otherwise = Nothing
        getIns False (x:xs) | x == '"'  = liftM2 (:) (lookup x instructions) $ getIns True xs
                            | isDigit x = liftM2 (:) (Just $ Num $ digitToInt x) $ getIns False xs
                            | otherwise = liftM2 (:) (lookup x instructions) $ getIns False xs
        getIns _ [] = Just []

initialize ::  String -> Program
initialize f = Program {
   stack      = fromListVector (Z :. stackSize) $ replicate stackSize Empty
  ,direction  = East
  ,fungeSpace = parse f
  ,currentPos = R.Z :. 0 :. 0
  }

main :: IO ()
main = do
  [fn] <- getArgs
  file <- readFile fn
  let prog = initialize file
  putStrLn $ show $ fungeSpace prog
