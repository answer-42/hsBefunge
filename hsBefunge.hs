{-# LANGUAGE TypeOperators #-}
module Main where

-- TODO
-- Add input commands
-- Make it fully Befunge 93 compliant
-- Refactor
-- Add error checking

import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import System.IO

import Control.Monad
import Control.Monad.State

import Data.Char (isAscii,isDigit,digitToInt,intToDigit,chr,ord)

import qualified Data.Array.Repa as R
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.Vector

data Instructions n s = 
      Not                -- !
      | TStringMode      -- "
      | Trampoline       -- #
      | Pop              -- $
      | Remainder        -- %
      | InputInt         -- &
      | Mult             -- *
      | Add              -- +
      | Min              -- - 
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
      | OutNum           -- .
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
  ,('-',Min)
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
  ,('.',OutNum)
  ,('|',IfNorthSouth)
  ,('~',InputChar)
  ,('@',Stop)
  ,(' ',Empty)]

data Direction = North | East | South | West deriving (Eq)

data ProgramState = Program {
   stack       :: [Instructions Int Char] -- Array V R.DIM1 (Instructions Int Char)
  ,direction   :: Direction  
  ,fungeSpace  :: Array V R.DIM2 Char
  ,currentPos  :: DIM2
  ,stringFlag  :: Bool
}

type BefungeState = StateT ProgramState IO () 

stackSize  = 1024 :: Int
befungeDim = (80,25) :: (Int,Int)

parse ::  String -> Array V R.DIM2 Char
parse x = fromListVector (Z :. snd befungeDim :. fst befungeDim)
                           $ addEmptyRows $ foldr ((++) . fillUp) [] $ lines x
  where fillUp l = if length l < fst befungeDim
                   then l ++ replicate (fst befungeDim-length l) ' '
                   else l
        
        addEmptyRows x = if length x < uncurry (*) befungeDim
                         then x++replicate (uncurry (*) befungeDim-length x) ' '
                         else x

initialize ::  String -> ProgramState
initialize f = Program {
   stack      = [] -- fromListVector (Z :. stackSize) $ replicate stackSize Empty
  ,direction  = East
  ,fungeSpace = parse f
  ,currentPos = Z :. 0 :. 0
  ,stringFlag = False
  }

getInst :: StateT ProgramState IO (Maybe (Instructions Int Char))
getInst = do p <- get
             let fS = fungeSpace p
                 cP = currentPos p
                 f  = stringFlag p
             return $ getIns' f (fS R.! cP)
  where getIns' :: Bool -> Char -> Maybe (Instructions Int Char)
        getIns' _ '"' = Just TStringMode
        getIns' False x | isDigit x = Just $ Num $ digitToInt x                
                        | otherwise = lookup x instructions
        getIns' True x | isAscii x = Just $ Character x
                       | otherwise = lookup x instructions


-- Stack
push :: Instructions Int Char ->  BefungeState
push x = do p <- get
            let s = stack p
            put p{stack=x:s}

pop :: StateT ProgramState IO (Instructions Int Char)
pop = do p <- get
         let s = stack p
             v = head s
         put p{stack=tail s}
         return v

-- Evaluator

goSouth :: BefungeState
goSouth = get >>= \p -> put p{direction=South}

goNorth :: BefungeState
goNorth = get >>= \p -> put p{direction=North}

goEast :: BefungeState
goEast = get >>= \p -> put p{direction=East}

goWest :: BefungeState
goWest = get >>= \p -> put p{direction=West}


move :: BefungeState 
move = let (x,y) = befungeDim
       in do p <- get
             let (Z:.i:.j) = currentPos p
                 newPos = case direction p of
                            East -> Z:.i:.(j+1)`mod`x
                            West -> Z:.i:.(j-1)`mod`x
                            North  -> Z:.(i-1)`mod`y:.j
                            South  -> Z:.(i+1)`mod`y:.j
             put p{currentPos=newPos}

notB :: BefungeState 
notB = do p <- get
          a <- pop
          let s = stack p
          case a of
            Num 0 -> put p{stack=Num 1:s}
            _     -> put p{stack=Num 0:s}

greaterThan :: BefungeState 
greaterThan = do p <- get
                 (Num a) <- pop
                 (Num b) <- pop
                 let s = stack p
                 case compare a b of
                   GT -> put p{stack=Num 1:s}
                   _  -> put p{stack=Num 0:s}

ifEastWest :: BefungeState 
ifEastWest = do p <- get
                (Num a) <- pop
                case a of
                  0 -> goEast
                  _ -> goWest

ifNorthSouth :: BefungeState 
ifNorthSouth = do p <- get
                  (Num a) <- pop
                  case a of
                    0 -> goSouth
                    _ -> goNorth

addB :: BefungeState
addB = do p <- get
          (Num a) <- pop
          (Num b) <- pop
          let s = stack p
          put p{stack=Num (a+b):s} 

multB :: BefungeState
multB = do p <- get
           (Num a) <- pop
           (Num b) <- pop
           let s = stack p
           put p{stack=Num (a*b):s}

minB :: BefungeState
minB = do p <- get
          (Num a) <- pop
          (Num b) <- pop
          let s = stack p
          put p{stack=Num (a-b):s}

remainderB :: BefungeState
remainderB = do p <- get
                (Num a) <- pop
                (Num b) <- pop
                let s = stack p
                case a of
                  0 -> put p{stack=Num 0:s}
                  _ -> put p{stack=Num (b`rem`a):s}

divB :: BefungeState
divB = do p <- get
          (Num a) <- pop
          (Num b) <- pop
          let s = stack p
          case a of
            0 -> put p{stack=Num 0:s}
            _ -> put p{stack=Num (b`quot`a):s}

toggleString :: BefungeState
toggleString = do p <- get
                  put p{stringFlag= not $ stringFlag p}

outChar :: BefungeState
outChar = do p <- get
             b <- pop
             case b of
               Num a       -> liftIO $ putChar $ chr a
               Character a -> liftIO $ putChar a

outNum :: BefungeState
outNum = do p <- get
            b <- pop
            case b of
              Num a       -> liftIO $ putChar $ intToDigit a
            liftIO $ putChar ' '

clear :: IO ()
clear = do x <- getChar
           if x == '\n'
           then return ()
           else clear

inChar :: BefungeState
inChar = liftIO getChar >>= (push . Character) >> liftIO clear
            
inInt :: BefungeState
inInt = liftIO getChar >>=  (push . Num . digitToInt) >> liftIO clear

evalBefunge :: BefungeState 
evalBefunge = do
  prog <- get
  cInstr <- getInst 
  case cInstr of
    -- Flow Control
    Just GoSouth      -> goSouth >> move >> evalBefunge
    Just GoNorth      -> goNorth >> move >> evalBefunge
    Just GoEast       -> goEast  >> move >> evalBefunge
    Just GoWest       -> goWest >> move >> evalBefunge
    Just Trampoline   -> move >> move >> evalBefunge

    Just Stop         -> liftIO exitSuccess

    -- Decision Making
    Just Not          -> notB >> move >> evalBefunge
    Just GreaterThan  -> greaterThan >> move >> evalBefunge
    Just IfEastWest   -> ifEastWest >> move >> evalBefunge
    Just IfNorthSouth -> ifNorthSouth >> move >> evalBefunge
    
    -- Number Crunching
    Just Add          -> addB >> move >> evalBefunge
    Just Mult         -> multB >> move >> evalBefunge
    Just Min          -> minB >> move >> evalBefunge
    Just Remainder    -> remainderB >> move >> evalBefunge
    Just Div          -> divB >> move >> evalBefunge

    -- Working with Strings 
    Just TStringMode -> toggleString >> move >> evalBefunge
    
    -- IO
    Just OutChar     -> outChar >> move >> evalBefunge
    Just OutNum      -> outNum >> move >> evalBefunge
    Just InputInt    -> inInt >> move >> evalBefunge
    Just InputChar   -> inChar >> move >> evalBefunge

    -- Stack Manipulations
    

    Just Empty       -> move >> evalBefunge
    Just x           -> push x >> move >> evalBefunge
    Nothing          -> liftIO exitFailure

main :: IO ()
main = do
  [fn] <- getArgs
  file <- readFile fn
  let prog = initialize file
  void $ runStateT evalBefunge prog
