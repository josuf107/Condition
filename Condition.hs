module Condition
    ( Checked
    , Condition
    , U
    , I
    , condition
    , test
    , check
    , uncheck
    , claim
    , cand
    , (&>)
    , cor
    , (|>)
    , pcond
    ) where

import Control.Applicative

-- c applies to v
newtype Checked c v = Checked { uncheck :: v }

-- all v such that c
newtype Condition c v = Condition { test :: v -> Bool }

newtype I c1 c2 = I (c1, c2)

newtype U c1 c2 = U (c1, c2)

condition :: (v -> Bool) -> Condition c v
condition = Condition

-- Condition intersect (c1 AND c2) c1 is checked first
cand :: Condition c1 v -> Condition c2 v -> Condition (I c1 c2) v
cand c1 c2 = Condition (\v -> test c1 v && test c2 v)

(&>) :: Condition c1 v -> Condition c2 v -> Condition (I c1 c2) v
(&>) = cand

-- Condition union (c1 OR c2)
cor :: Condition c1 v -> Condition c2 v -> Condition (U c1 c2) v
cor c1 c2 = Condition (\v -> test c1 v || test c2 v)

(|>) :: Condition c1 v -> Condition c2 v -> Condition (U c1 c2) v
(|>) = cor

check :: Condition c v -> v -> Maybe (Checked c v)
check c v = if test c v then Just (Checked v) else Nothing

-- claim c for v
claim :: Condition c v -> v -> Checked c v
claim c v = if test c v then Checked v else error "Falsifiable claim!"

pcond :: Condition c v -> v -> (Checked c v -> v2) -> Maybe v2
pcond cond v f = f <$> check cond v
