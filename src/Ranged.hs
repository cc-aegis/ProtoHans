module Ranged (Ranged, getStart, getEnd, getContent) where

import Data.Semigroup

data Ranged a = Ranged { start :: Min Int, end :: Max Int, content :: a }
    deriving (Show)

instance Functor Ranged where
    fmap f (Ranged { start, end, content }) = Ranged { start, end, content = f content }

instance Applicative Ranged where
    pure content = Ranged { start = mempty, end = mempty, content }
    Ranged { start, end, content = f } <*> Ranged { start = start', end = end', content } = Ranged { start = start <> start', end = end <> end', content = f content }

instance Semigroup a => Semigroup (Ranged a) where
  Ranged { start, end, content } <> Ranged { start = start', end = end', content = content' } = Ranged { start = start <> start', end = end <> end', content = content <> content' }

getStart :: Ranged a -> Int
getStart Ranged { start = Min start } = start

getEnd :: Ranged a -> Int
getEnd Ranged { end = Max end } = end

getContent :: Ranged a -> a
getContent Ranged { content } = content
