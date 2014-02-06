module Example.Library
    ( Even
    , isEven
    , three
    , Nonzero
    , isNonzero
    , Validated
    , validate
    , indexable
    , index
    , half
    , double
    , timesEven
    , divide
    , halfOrThree
    ) where

import Condition

data Even = Even

data Nonzero = Nonzero

data Validated = Validated

data Indexable = Indexable

data Three = Three

three :: (Num n, Eq n) => Condition Three n
three = condition (==3)

validated :: Condition Validated String
validated = condition ((>4) . length)

validate :: String -> Checked Validated String
validate s = claim Validated (s ++ "     ")

isEven :: Integral n => Condition Even n
isEven = condition even

isNonzero :: (Eq n, Num n) => Condition Nonzero n
isNonzero = condition (/=0)

indexable :: Condition Indexable ([a], Int)
indexable = condition (\(l, i) -> i < length l)

index :: Checked Indexable ([a], Int) -> a
index = uncurry (!!) . uncheck

half :: Integral n => Checked Even n -> n
half n = uncheck n `div` 2

halfOrThree :: Integral n => Checked (U Even Three) n -> n
halfOrThree n = if uncheck n == 3 then 3 else uncheck n `div` 2

timesEven :: Integral n => n -> Checked Even n -> Checked Even n
timesEven a b = claim Even $ a * uncheck b

double :: Integral n => n -> Checked Even n
double n = claim Even (2 * n)

divide :: Fractional n => n -> Checked Nonzero n -> n
divide x y = x / uncheck y
