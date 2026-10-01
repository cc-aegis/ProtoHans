module Ranged (Ranged(..), getStart, getEnd, getContent) where

import Data.Semigroup

data Ranged a = Ranged (Min Int) (Max Int) a
    deriving (Eq, Show)

instance Functor Ranged where
    fmap f (Ranged start end content) = Ranged start end $ f content

instance Applicative Ranged where
    pure content = Ranged mempty mempty content
    (Ranged start end f) <*> (Ranged start' end' content) = Ranged (start <> start') (end <> end') (f content)

instance Monad Ranged where
    (Ranged start end content) >>= f = f content >>= Ranged start end

instance Semigroup a => Semigroup (Ranged a) where
  (Ranged start end content) <> (Ranged start' end' content') = Ranged (start <> start') (end <> end') (content <> content')

getStart :: Ranged a -> Int
getStart (Ranged (Min start) _ _) = start

getEnd :: Ranged a -> Int
getEnd (Ranged _ (Max end) _) = end

getContent :: Ranged a -> a
getContent (Ranged _ _ content) = content
