{-# OPTIONS_GHC -fno-warn-orphans #-}

module Todo.Enum where

instance Bounded a => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just maxBound

instance Enum a => Enum (Maybe a) where
  toEnum 0 = Nothing
  toEnum x = Just (toEnum (x - 1))

  fromEnum Nothing = 0
  fromEnum (Just x) = 1 + fromEnum x

enum :: (Bounded a, Enum a) => [a]
enum = [minBound .. maxBound]
