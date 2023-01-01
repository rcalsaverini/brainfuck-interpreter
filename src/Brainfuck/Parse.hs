module Brainfuck.Parse where

import Text.Parsec
import Brainfuck.Type

incrementRegister :: Parsec String () Brainfuck
incrementRegister = do 
    _ <- char '>'
    return IncrementRegister

decrementRegister :: Parsec String () Brainfuck
decrementRegister = do 
    _ <- char '<'
    return DecrementRegister

increment :: Parsec String () Brainfuck
increment = do 
    _ <- char '+'
    return Increment

decrement :: Parsec String () Brainfuck
decrement = do 
    _ <- char '-'
    return Decrement

output :: Parsec String () Brainfuck
output = do 
    _ <- char '.'
    return Output

input :: Parsec String () Brainfuck
input = do 
    _ <- char ','
    return Input


loop :: Parsec String () Brainfuck
loop = do 
    _ <- char '['
    internalCommands <- instructions
    _ <- char ']'
    return $ Loop internalCommands



instructions :: Parsec String () [Brainfuck]
instructions = many (incrementRegister <|> decrementRegister <|> increment <|> decrement <|> output <|> input <|> loop)

parseBrainfuck :: String -> Either ParseError [Brainfuck]
parseBrainfuck = parse instructions "Brainfuck"