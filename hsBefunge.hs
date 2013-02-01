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

import Control.Monad (unless)
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
   stack       :: [Maybe (Instructions Int Char)] -- Array V R.DIM1 (Instructions Int Char)
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

getIns' :: Bool -> Char -> Maybe (Instructions Int Char)
getIns' _ '"' = Just TStringMode
getIns' False x | isDigit x = Just $ Num $ digitToInt x                
                | otherwise = lookup x instructions
getIns' True x | isAscii x = Just $ Character x
               | otherwise = lookup x instructions


-- Stack
push :: Maybe (Instructions Int Char) ->  BefungeState
push x = do p <- get
            let s = stack p
            put p{stack=x:s}

pop :: StateT ProgramState IO (Maybe (Instructions Int Char))
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
           Just (Num 0) -> put p{stack=(Just . Num) 1:s}
           Nothing    -> put p{stack=Nothing:s}
           _          -> put p{stack=(Just . Num) 0:s}

greaterThan :: BefungeState 
greaterThan = do p <- get
                 Just (Num a) <- pop
                 Just (Num b) <- pop
                 let s = stack p
                 case compare a b of
                   GT -> put p{stack=(Just . Num) 1:s}
                   _  -> put p{stack=(Just . Num) 0:s}

ifEastWest :: BefungeState 
ifEastWest = do p <- get
                Just (Num a) <- pop
                case a of
                  0 -> goEast
                  _ -> goWest

ifNorthSouth :: BefungeState 
ifNorthSouth = do p <- get
                  Just (Num a) <- pop
                  case a of
                    0 -> goSouth
                    _ -> goNorth

addB :: BefungeState
addB = do p <- get
          Just (Num a) <- pop
          Just (Num b) <- pop
          let s = stack p
          put p{stack=(Just . Num) (a+b):s} 

multB :: BefungeState
multB = do p <- get
           Just (Num a) <- pop
           Just (Num b) <- pop
           let s = stack p
           put p{stack=(Just . Num) (a*b):s}

minB :: BefungeState
minB = do p <- get
          Just (Num a) <- pop
          Just (Num b) <- pop
          let s = stack p
          put p{stack=(Just . Num) (a-b):s}

remainderB :: BefungeState
remainderB = do p <- get
                Just (Num a) <- pop
                Just (Num b) <- pop
                let s = stack p
                case a of
                  0 -> put p{stack=(Just . Num) 0:s}
                  _ -> put p{stack=(Just .Num) (b`rem`a):s}

divB :: BefungeState
divB = do p <- get
          Just (Num a) <- pop
          Just (Num b) <- pop
          let s = stack p
          case a of
            0 -> put p{stack=(Just . Num) 0:s}
            _ -> put p{stack=(Just . Num) (b`quot`a):s}

toggleString :: BefungeState
toggleString = do p <- get
                  put p{stringFlag= not $ stringFlag p}

outChar :: BefungeState
outChar = do p <- get
             Just b <- pop
             case b of
               Num a       -> liftIO $ putChar $ chr a
               Character a -> liftIO $ putChar a

outNum :: BefungeState
outNum = do p <- get
            Just b <- pop
            case b of
              Num a       -> liftIO $ putChar $ intToDigit a
            liftIO $ putChar ' '

clear :: IO ()
clear = do x <- getChar
           unless (x == '\n') clear

inChar :: BefungeState
inChar = liftIO getChar >>= (push . Just . Character) >> liftIO clear
            
inInt :: BefungeState
inInt = liftIO getChar >>=  (push . Just . Num . digitToInt) >> liftIO clear

dup :: BefungeState
dup = do x <- pop
         push x
         push x

swap :: BefungeState
swap = do x <- pop
          y <- pop
          push y
          push x

getB :: BefungeState
getB = do Just (Num y) <- pop
          Just (Num x) <- pop

          p <- get
          let fS = fungeSpace p

          push $ getIns' False $ fS R.! (Z :. y :. x) 

putB :: BefungeState
putB = do Just (Num y) <- pop
          Just (Num x) <- pop
          v <- pop

          p <- get
          let fS = fungeSpace p

          put p{fungeSpace=update fS (Z :. y :. x) v}
  where update :: Array V R.DIM2 Char -> R.DIM2 -> Maybe (Instructions Int Char) -> Array V R.DIM2 Char
        update fS a@(Z:.y:.x) i = R.computeS $ R.traverse fS id (\f b  -> if a == b 
                                                                          then getV i
                                                                          else f b)
        getV x = case x of
                   Just (Num a)       -> intToDigit a
                   Just (Character a) -> a


evalBefunge :: BefungeState 
evalBefunge = do
  cInstr <- getInst 
  case cInstr of
    -- Flow Control
    Just GoSouth      -> goSouth
    Just GoNorth      -> goNorth
    Just GoEast       -> goEast
    Just GoWest       -> goWest
    Just Trampoline   -> move

    Just Stop         -> liftIO exitSuccess

    -- Decision Making
    Just Not          -> notB
    Just GreaterThan  -> greaterThan
    Just IfEastWest   -> ifEastWest
    Just IfNorthSouth -> ifNorthSouth
    
    -- Number Crunching
    Just Add          -> addB
    Just Mult         -> multB
    Just Min          -> minB
    Just Remainder    -> remainderB
    Just Div          -> divB

    -- Working with Strings 
    Just TStringMode -> toggleString
    
    -- IO
    Just OutChar     -> outChar
    Just OutNum      -> outNum
    Just InputInt    -> inInt
    Just InputChar   -> inChar

    -- Stack Manipulations
    Just Pop         -> void pop 
    Just Dup         -> dup    
    Just Swap        -> swap

    -- Funge-Space Storage
 --   Just Get         -> getB
 --   Just Put         -> putB

    Just Empty       -> get >>= put 
    Just x           -> push $ Just x
    Nothing          -> liftIO exitFailure
  move
  evalBefunge

main :: IO ()
main = do
  [fn] <- getArgs
  file <- readFile fn
  let prog = initialize file
  void $ runStateT evalBefunge prog
