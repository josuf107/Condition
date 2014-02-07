module Example.Client where

import Control.Applicative

import Example.Library
import Condition

client :: String
client = stripValid $ validate "eel"

stripValid :: Checked Validated String -> String
stripValid = drop 7 . uncheck

t1 :: Maybe Integer
t1 = halfOrThree <$> check (isEven |> three) 3

t2 :: Maybe Integer
t2 = halfOrThree <$> check (isEven |> three) 6

t3 :: Maybe Integer
t3 = halfOrThree <$> check (isEven |> three) 7
