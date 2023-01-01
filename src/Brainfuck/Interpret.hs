{-# LANGUAGE TemplateHaskell #-}

module Brainfuck.Interpret where

import Control.Monad.State
import Brainfuck.Type
import Control.Lens

data Registers a = Registers {_left :: [a], _current :: a, _right :: [a]} deriving(Show, Eq)
newtype BrainfuckState = BrainfuckState {_registers :: Registers Int} deriving(Show, Eq)

makeLenses ''Registers
makeLenses ''BrainfuckState

infixr 4 <==
infixr 4 <~~
(<==) :: ASetter s t a b -> Getting b s b -> s -> t
l <== g = \x -> l .~ x ^. g $ x

(<~~) :: ASetter s t a b -> Getting (a -> b) s (a -> b) -> s -> t
l <~~ g = \x -> over l (x ^. g) x

startState :: Int -> BrainfuckState
startState n = BrainfuckState {_registers = Registers {_left = [], _current = 0, _right = replicate (n - 1) 0}}

incrementRegister :: BrainfuckState -> BrainfuckState
incrementRegister = over registers $ fixLeft . fixCurr . fixRight
    where fixLeft = left <~~ current . to (:)
          fixCurr = current <== right . to head
          fixRight = over right tail


decrementRegister :: BrainfuckState -> BrainfuckState
decrementRegister = over registers $ fixLeft . fixCurr . fixRight
    where fixLeft = over left tail
          fixCurr = current <== left . to head
          fixRight = right <~~ current . to (:)


increment :: BrainfuckState -> BrainfuckState
increment = over registers (current +~ 1)

decrement :: BrainfuckState -> BrainfuckState
decrement = over registers (current -~ 1)

output :: BrainfuckState -> String
output = (^. registers . current . to show)

input :: Int -> BrainfuckState -> BrainfuckState
input n = over registers (current .~ n)

loop :: Monad m => m Int -> (String -> m ()) -> [Brainfuck] -> BrainfuckState -> StateT BrainfuckState m ()
loop r w xs u = case u ^. registers . current of
        0 -> return ()
        _ -> interpret r w xs >> loop r w xs u

interpretOne :: Monad m => m Int -> (String -> m ()) -> Brainfuck -> StateT BrainfuckState m ()
interpretOne r w x = case x of
        IncrementRegister -> modify incrementRegister
        DecrementRegister -> modify decrementRegister
        Increment -> modify increment
        Decrement -> modify decrement
        Output -> do
            u <- get 
            lift $ w (output u)
        Input -> do
            val <- lift r 
            modify (input val)
        Loop ts -> do
            u <- get
            loop r w ts u

interpret :: Monad m => m Int -> (String -> m ()) -> [Brainfuck] -> StateT BrainfuckState m ()
interpret _ _ [] = return ()
interpret r w [x] = interpretOne r w x
interpret r w (x:xs) = interpretOne r w x >> interpret r w xs