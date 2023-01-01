module Main where

import Brainfuck.Type (Brainfuck)
import Brainfuck.Interpret (interpret, startState)
import Brainfuck.Parse (parseBrainfuck)
import Control.Monad.State

parseInstructions :: String -> [Brainfuck]
parseInstructions string = case parseBrainfuck string of 
    Left err -> error $ "Parse error: " ++ show err
    Right instructions -> instructions


interpretIO :: [Brainfuck] -> IO ()
interpretIO program = evalStateT run (startState 300)
    where 
        run = interpret getInt putStrLn program
        getInt = do
            putStr "Input: "
            input <- getLine
            return $ read input



main :: IO ()
main = putStrLn "asdasdas"