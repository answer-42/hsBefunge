{-# LANGUAGE TypeOperators #-}
module Main where

-- TODO
-- Add error checking

import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import System.IO

import Control.Monad (unless)
import Control.Monad.State

-- import Control.Arrow ((&&&))

import Data.Char (isAscii,isDigit,digitToInt,intToDigit,chr,ord)
import qualified Data.Tuple as T
-- import Data.Function (on)

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
   stack       :: Maybe [Instructions Int Char] -- Array V R.DIM1 (Instructions Int Char)
  ,direction   :: Direction  
  ,fungeSpace  :: Maybe (Array V R.DIM2 Char)
  ,currentPos  :: DIM2
  ,stringFlag  :: Bool
}

type BefungeState = StateT ProgramState IO () 

stackSize  = 1024 :: Int
befungeDim = (80,25) :: (Int,Int)

parse ::  String -> Maybe (Array V R.DIM2 Char)
parse x = Just $ fromListVector (Z :. snd befungeDim :. fst befungeDim)
                           $ addEmptyRows $ foldr ((++) . fillUp) [] $ lines x
  where fillUp l = if length l < fst befungeDim
                   then l ++ replicate (fst befungeDim-length l) ' '
                   else l
        
        addEmptyRows x = if length x < uncurry (*) befungeDim
                         then x++replicate (uncurry (*) befungeDim-length x) ' '
                         else x

initialize ::  String -> ProgramState
initialize f = Program {
   stack      = Just [] -- fromListVector (Z :. stackSize) $ replicate stackSize Empty
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
             return $ case fS of
                        Just fS -> getIns' f (fS R.! cP)
                        Nothing -> Nothing

getIns' :: Bool -> Char -> Maybe (Instructions Int Char)
getIns' _ '"' = Just TStringMode
getIns' False x | isDigit x = Just $ Num $ digitToInt x                
                | otherwise = lookup x instructions
getIns' True x | isAscii x = Just $ Character x
               | otherwise = lookup x instructions


(+|) :: Instructions Int Char -> Maybe [Instructions Int Char] -> Maybe [Instructions Int Char]
(+|) x = liftM (x:)

(|+|) = liftM2 (:)

-- Stack
push :: Maybe (Instructions Int Char) ->  BefungeState
push x = do p <- get
            let s = stack p
            put p{stack=x|+|s}

pop :: StateT ProgramState IO (Maybe (Instructions Int Char))
pop = do p <- get
         let s = stack p
             v = liftM head s
         put p{stack=liftM tail s}
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
           Just (Num 0) -> put p{stack=Num 1+|s}
           Nothing    -> put p{stack=Nothing}
           _          -> put p{stack=Num 0+|s}

greaterThan :: BefungeState 
greaterThan = do p <- get
                 a <- pop
                 b <- pop
                 let s = stack p
                 case (a,b) of
                   (Just (Num a), Just (Num b)) -> case compare a b of
                                                     GT -> put p{stack=Num 1+|s}
                                                     _  -> put p{stack=Num 0+|s}
                   _                            -> put p{stack=Nothing} 

ifEastWest :: BefungeState 
ifEastWest = do p <- get
                a <- pop
                case a of
                  Just (Num 0) -> goEast
                  Nothing      -> return ()
                  _            -> goWest

ifNorthSouth :: BefungeState 
ifNorthSouth = do p <- get
                  a <- pop
                  case a of
                    Just (Num 0) -> goSouth
                    Nothing      -> return () 
                    _            -> goNorth

addB :: BefungeState
addB = do p <- get
          a <- pop
          b <- pop
          let s = stack p
          put p{stack=liftM2 addN a b |+| s}
  where addN (Num a) (Num b) = Num $ a+b

multB :: BefungeState
multB = do p <- get
           a <- pop
           b <- pop
           let s = stack p
           put p{stack=liftM2 multN a b |+| s}
  where multN (Num a) (Num b) = Num $ a*b

minB :: BefungeState
minB = do p <- get
          a <- pop
          b <- pop
          let s = stack p
          put p{stack=liftM2 minN a b |+| s}
  where minN (Num a) (Num b) = Num $ a-b

remainderB :: BefungeState
remainderB = do p <- get
                a <- pop
                b <- pop
                let s = stack p
                put p{stack=liftM2 remN a b |+| s}
  where remN (Num 0) (Num _) = Num 0 
        remN (Num a) (Num b) = Num $ b `rem` a

divB :: BefungeState
divB = do p <- get
          a <- pop
          b <- pop
          let s = stack p
          put p{stack=liftM2 divN a b |+| s}
  where divN (Num 0) (Num _) = Num 0 
        divN (Num a) (Num b) = Num $ b `quot` a

toggleString :: BefungeState
toggleString = do p <- get
                  put p{stringFlag= not $ stringFlag p}

outChar :: BefungeState
outChar = do p <- get
             b <- pop
             case b of
               Just (Num a)       -> liftIO $ putChar $ chr a
               Just (Character a) -> liftIO $ putChar a
               Nothing            -> return () 

outNum :: BefungeState
outNum = do p <- get
            b <- pop
            case b of
              Just (Num a)       -> liftIO $ putChar $ intToDigit a
              _                  -> return () 
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
getB = do y <- pop
          x <- pop

          p <- get
          let fS = fungeSpace p
          case (x,y,fS) of
            (Just (Num j), Just (Num i), Just fS) -> push $ getIns' False $ fS R.! (Z :. i :. j)
            _                                     -> push Nothing

putB :: BefungeState
putB = do y <- pop
          x <- pop
          v <- pop

          p <- get
          let fS = fungeSpace p
          
          case (x,y, fS) of
            (Just (Num j), Just (Num i), Just fS) -> put p{fungeSpace=update fS (Z :. i :. j) v}
            _                                     -> return () 

  where update :: Array V R.DIM2 Char -> R.DIM2 -> Maybe (Instructions Int Char) -> Maybe (Array V R.DIM2 Char)
        update fS a@(Z:.y:.x) i = do v <- getV i 
                                     return $ R.computeS $ R.traverse fS id (\f b  -> if a == b 
                                                                                      then v
                                                                                      else f b)
        getV (Just x) = lookup x $ map T.swap instructions
        getV _        = Nothing 
        
        --case x of
                   -- Just (Num a)       -> intToDigit a
                   -- Just (Character a) -> a
                   -- _                  -> argumentError

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
    Just Get         -> getB
    Just Put         -> putB

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
