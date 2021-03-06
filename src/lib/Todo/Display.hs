{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}

module Todo.Display where

import Data.Foldable
import Data.List
import Data.Proxy

import Data.Typeable (Typeable, typeRep)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, natVal, symbolVal)
import Numeric (fromRat, showFFloat)

import Data.Text (Text)
import qualified Data.Text as Text


-- | Human-readable text.
class Display (a :: *) where
  display :: a -> Text

instance Display a => Display (Maybe a) where
  display = display . toList

instance Display a => Display [a] where
  display [] = "-"
  display xs = Text.intercalate ", " $ fmap display xs

instance (Display a, Display b) => Display (a, b) where
  display (x, y) = display x <> ", " <> display y

instance (Display a, Display b) => Display (Either a b) where
  display (Left x) = display x
  display (Right y) = display y

instance Display Text where
  display = id

instance Display Int where
  display = display . toInteger

instance Display Integer where
  display n =
    sign <> grouped n
    where

    sign = if n < 0 then "-" else ""

    grouped =
      Text.pack . reverse . intercalate "," . triples . reverse . show . abs

    triples (x:y:z:rest) = [x,y,z] : triples rest
    triples [] = []
    triples xs = [xs]


-- | Specify a decimal precision to display.
newtype Precision (n :: Nat) (a :: *) = Precision a

instance (KnownNat n, Real a) => Display (Precision n a) where
  display (Precision x) =
    sign <> whole <> "." <> fraction
    where

    sign = if x < 0 then "-" else ""

    (w, f) = properFraction @_ @Integer (toRational $ abs x)

    whole = display w

    fraction =
      Text.dropEnd 1 . Text.drop 2 . Text.pack $
        showFFloat @Double
          (Just . succ . fromIntegral $ natVal @n Proxy)
          (fromRat f)
          ""


-- | Derive a display instance from a show instance.
newtype DeriveDisplay (prefix :: k) (a :: *) = DeriveDisplay a

instance Show a => Display (DeriveDisplay '() a) where
  display (DeriveDisplay x) = Text.pack (show x)

instance (KnownNat n, Show a) => Display (DeriveDisplay n a) where
  display (DeriveDisplay x) =
    Text.drop (fromIntegral $ natVal @n Proxy) . Text.pack $ show x

instance (KnownSymbol s, Show a) => Display (DeriveDisplay s a) where
  display (DeriveDisplay x) =
    Text.drop (length $ symbolVal @s Proxy) . Text.pack $ show x

instance (Typeable t, Show a, t ~ a) => Display (DeriveDisplay t a) where
  display (DeriveDisplay x) =
    Text.drop (length . show $ typeRep @_ @t Proxy) . Text.pack $ show x
