{-# LANGUAGE TypeOperators #-}
module Main where

import System.Environment (getArgs)

import Control.Monad (mapM,liftM,liftM2)
import Control.Monad.State

import Data.Char (isAscii,isDigit,digitToInt)

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
  ,('|',IfNorthSouth)
  ,('~',InputChar)
  ,('@',Stop)
  ,(' ',Empty)]

data Direction = North | East | South | West deriving (Eq)

data ProgramState = Program {
   stack       :: [Instructions Int Char] -- Array V R.DIM1 (Instructions Int Char)
  ,direction   :: Direction  
  ,fungeSpace  :: Maybe (Array V R.DIM2 (Instructions Int Char))
  ,currentPos  :: DIM2
}

type BefungeState = StateT ProgramState IO () 

stackSize  = 1024 :: Int
befungeDim = (80,25) :: (Int,Int)

parse ::  String -> Maybe (Array V R.DIM2 (Instructions Int Char))
parse x = liftM (fromListVector (Z :. snd befungeDim :. fst befungeDim)) 
                           $ getIns False $ addEmptyRows $ foldr ((++) . fillUp) [] $ lines x
  where fillUp l = if length l < fst befungeDim
                   then l ++ replicate (fst befungeDim-length l) ' '
                   else l
        
        addEmptyRows x = if length x < uncurry (*) befungeDim
                         then x++replicate (uncurry (*) befungeDim-length x) ' '
                         else x

        getIns :: Bool -> String -> Maybe [Instructions Int Char ]
        getIns True (x:xs) | x == '"'  = liftM2 (:) (lookup x instructions) $ getIns False xs
                           | isAscii x = liftM2 (:) (Just $ Character x) $ getIns True xs
                           | otherwise = Nothing
        getIns False (x:xs) | x == '"'  = liftM2 (:) (lookup x instructions) $ getIns True xs
                            | isDigit x = liftM2 (:) (Just $ Num $ digitToInt x) $ getIns False xs
                            | otherwise = liftM2 (:) (lookup x instructions) $ getIns False xs
        getIns _ [] = Just []

initialize ::  String -> ProgramState
initialize f = Program {
   stack      = [] -- fromListVector (Z :. stackSize) $ replicate stackSize Empty
  ,direction  = East
  ,fungeSpace = parse f
  ,currentPos = Z :. 0 :. 0
  }

-- Stack
push :: Instructions Int Char ->  BefungeState
push x = do p <- get
            let s = stack p
            put p{stack=(s++[x])}


-- pop :: BefungeState (Instructions Int Char)
pop = undefined

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

notB :: Instructions Int Char -> Instructions Int Char 
notB (Num 0) = Num 1
notB _     = Num 0

greaterThan :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
greaterThan (Num a) (Num b) | a >= b    = Num 1
                            | otherwise = Num 0

ifEastWest :: Instructions Int Char -> BefungeState 
ifEastWest (Num 0) = goEast
ifEastWest _       = goWest

ifNorthSouth :: Instructions Int Char -> BefungeState 
ifNorthSouth (Num 0) = goSouth
ifNorthSouth _       = goNorth

addB :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
addB (Num a) (Num b) = Num $ a+b
addB _ _             = undefined

multB :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
multB (Num a) (Num b) = Num $ a*b
multB _ _             = undefined

minB :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
minB (Num a) (Num b) = Num $ a-b
minB _ _             = undefined

remainderB :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
remainderB (Num 0) (Num b) = Num 0
remainderB (Num a) (Num b) = Num $ b `rem` a
remainderB _ _             = undefined

divB :: Instructions Int Char -> Instructions Int Char -> Instructions Int Char
divB (Num 0) (Num b) = Num 0
divB (Num a) (Num b) = Num $ b `quot` a
divB _ _             = undefined


evalBefunge :: BefungeState 
evalBefunge = do
  prog <- get
  let cStack = stack prog
      cDir   = direction prog
      cFungeSpace = fungeSpace prog
      cPos   = currentPos prog
      
      cInstr = (cFungeSpace >>= \x -> return $ x R.! cPos)
  liftIO$ print cInstr
  case cInstr of
    -- Flow Control
    Just GoSouth      -> goSouth >> move >> evalBefunge
    Just GoNorth      -> goNorth >> move >> evalBefunge
    Just GoEast       -> goEast  >> move >> evalBefunge
    Just GoWest       -> goWest >> move >> evalBefunge
    Just Trampoline   -> move >> move >> evalBefunge

    Just Stop         -> return ()

    -- Decision Making
    Just Not          -> (push $ notB pop) >> move >> evalBefunge
    Just GreaterThan  -> (push $ pop `greaterThan` pop) >> move >> evalBefunge
    Just IfEastWest   -> ifEastWest pop >> move >> evalBefunge
    Just IfNorthSouth -> ifNorthSouth pop >> move >> evalBefunge
    
    -- Number Crunching
    Just Add          -> (push $ pop `addB` pop) >> move >> evalBefunge
    Just Mult         -> (push $ pop `multB` pop) >> move >> evalBefunge
    Just Min          -> (push $ pop `minB` pop) >> move >> evalBefunge
    Just Remainder    -> (push $ pop `remainderB` pop) >> move >> evalBefunge
    Just Div          -> (push $ pop `divB` pop) >> move >> evalBefunge

    -- Working with Strings 
    Just TStringMode -> move >> evalBefunge
 --   Just OutChar     -> (liftIO $ print pop) >> move >> evalBefunge

    -- Stack Manipulations
    
    Just Empty       -> move >> evalBefunge
    _       -> undefined

main :: IO ()
main = do
  [fn] <- getArgs
  file <- readFile fn
  let prog = initialize file
  void $ runStateT evalBefunge prog
