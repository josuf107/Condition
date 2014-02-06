module Example.Client where

import Control.Applicative

import Example.Library
import Condition

client :: Char
client = danger $ validate "eel"

danger :: Checked Validated String -> Char
danger s = head . drop 4 . uncheck $ s

t1 :: Maybe Integer
t1 = halfOrThree <$> (check (isEven |> three) 3)

t2 :: Maybe Integer
t2 = halfOrThree <$> (check (isEven |> three) 6)

t3 :: Maybe Integer
t3 = halfOrThree <$> (check (isEven |> three) 7)
