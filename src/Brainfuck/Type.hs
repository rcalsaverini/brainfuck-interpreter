{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Brainfuck.Type where

data Brainfuck
  = IncrementRegister
  | DecrementRegister
  | Increment
  | Decrement
  | Output
  | Input
  | Loop [Brainfuck]

instance Show Brainfuck where
  show IncrementRegister = "IncrementRegister"
  show DecrementRegister = "DecrementRegister"
  show Increment = "Increment"
  show Decrement = "Decrement"
  show Output = "Output"
  show Input = "Input"
  show (Loop xs) = "Loop " ++ show xs