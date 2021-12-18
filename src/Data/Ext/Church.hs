{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ext.Church
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A pair-like data type to represent a 'core' type that has extra information
-- as well.
--
-- implementation is essentially a church encoded pair. The idea would
-- be that in many cases we are just using () as ext, and GHC will
-- hopefully optimimize that away.
--
--------------------------------------------------------------------------------
module Data.Ext.Church
  ( (:+)((:+)), core, extra, _core, _extra, ext
  ) where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Apply
import Data.Bitraversable
-- import Data.Default
import Data.Functor.Apply (liftF2)
import Data.Semigroup.Bifoldable
import Data.Semigroup.Bitraversable
import GHC.Generics
import Test.QuickCheck

import qualified GHC.Read
import qualified GHC.Show
import qualified GHC.Base
import qualified Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex

--------------------------------------------------------------------------------

-- type core :+ extra = Ext core extra
infixr 1 :+

pattern (:+) :: core -> extra -> core :+ extra
pattern core :+ extra <- (destruct -> ~(core,extra))
  where
    core' :+ extra' = construct core' extra'
{-# COMPLETE (:+) #-}

-- | Our Ext type that represents the core datatype core extended with
-- extra information of type 'extra'.
newtype core :+ extra = Ext (forall r. (core -> extra -> r) -> r)


-- | Constructs and ext
construct     :: core -> extra -> core :+ extra
construct c e = Ext $ \f -> f c e
{-# INLINE construct #-}

-- | Destructs an ext into a pair
destruct         :: core :+ extra -> (core, extra)
destruct (Ext k) = k $ \c e -> (c,e)
{-# INLINE destruct #-}

instance Bifunctor (:+) where
  bimap f g (Ext k) = k $ \c e -> construct (f c) (g e)
instance Bifoldable (:+) where
  bifoldMap f g (Ext k) = k $ \c e -> f c <> g e
instance Bitraversable (:+) where
  bitraverse f g (Ext k) = k $ \c e -> construct <$> f c <*> g e

-- | Lens access to the core of an extended value.
core :: Lens (core :+ extra) (core' :+ extra) core core'
core = lens _core (\(_ :+ e) c -> c :+ e)
{-# INLINE core #-}

-- | Lens access to the extra part of an extended value.
extra :: Lens (core :+ extra) (core :+ extra') extra extra'
extra = lens _extra (\(c :+ _) e -> c :+ e)
{-# INLINE extra #-}

-- | Access the core of an extended value.
_core         :: (core :+ extra) -> core
_core (Ext k) = k $ \c _ -> c
{-# INLINABLE _core #-}

-- | Access the extra part of an extended value.
_extra :: (core :+ extra) -> extra
_extra (Ext k) = k $ \_ e -> e
{-# INLINABLE _extra #-}

-- | Tag a value with the unit type.
ext   :: a -> a :+ ()
ext x = x :+ ()
{-# INLINABLE ext #-}

instance (Eq core, Eq extra) => Eq (core :+ extra) where
  (c :+ e) == (c' :+ e') = c == c' && e == e'

instance (Ord core, Ord extra) => Ord (core :+ extra) where
  (c :+ e) `compare` (c' :+ e') = c `compare` c' <> e `compare` e'

instance (Bounded core, Bounded extra) => Bounded (core :+ extra) where
  minBound = minBound :+ minBound
  maxBound = maxBound :+ maxBound

instance Biapply (:+) where
  (f :+ g) <<.>> (c :+ e) = f c :+ g e

instance Biapplicative (:+) where
  bipure = (:+)
  (f :+ g) <<*>> (c :+ e) = f c :+ g e

instance Bifoldable1 (:+)

instance Bitraversable1 (:+) where
  bitraverse1 f g (c :+ e) = liftF2 (:+) (f c) (g e)

instance (Semigroup core, Semigroup extra) => Semigroup (core :+ extra) where
  (c :+ e) <> (c' :+ e') = c <> c' :+ e <> e'

instance (ToJSON core, ToJSON extra) => ToJSON (core :+ extra) where
  -- toJSON     (c :+ e) = toJSON     (c,e)
  -- toEncoding (c :+ e) = toEncoding (c,e)
  toJSON     (c :+ e) = object ["core" .= c, "extra" .= e]
  toEncoding (c :+ e) = pairs  ("core" .= c <> "extra" .= e)

instance (FromJSON core, FromJSON extra) => FromJSON (core :+ extra) where
  -- parseJSON = fmap (\(c,e) -> c :+ e) . parseJSON
  parseJSON (Object v) = (:+) <$> v .: "core" <*> v .: "extra"
  parseJSON invalid    = typeMismatch "Ext (:+)" invalid

instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary

-- instance (Default a, Default b) => Default (a :+ b) where
--   def = def :+ def

--------------------------------------------------------------------------------
-- Some derived instances I let ghc dump

instance Generic (core :+ extra) where
  type instance Rep (core :+ extra)
    = D1
      ('MetaData
         ":+" "Data.Ext" "hgeometry-combinatorial" 'False)
      (C1
         ('MetaCons ":+" ('InfixI 'RightAssociative 1) 'False)
         (S1
            ('MetaSel
               'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
            (Rec0 core)
          :*: S1
                ('MetaSel
                   'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 extra)))

  from x_aka8
    = GHC.Generics.M1
        (case x_aka8 of {
           (:+) g1_aka9 g2_akaa
             -> GHC.Generics.M1
                  ((GHC.Generics.:*:)
                     (GHC.Generics.M1 (GHC.Generics.K1 g1_aka9))
                     (GHC.Generics.M1 (GHC.Generics.K1 g2_akaa))) })
  to (GHC.Generics.M1 x_akab)
    = case x_akab of {
        (GHC.Generics.M1 ((GHC.Generics.:*:) (GHC.Generics.M1 (GHC.Generics.K1 g1_akac))
                                             (GHC.Generics.M1 (GHC.Generics.K1 g2_akad))))
          -> (:+) g1_akac g2_akad }

instance (NFData core, NFData extra) => NFData (core :+ extra) where

instance (GHC.Show.Show core, GHC.Show.Show extra) =>
         GHC.Show.Show (core :+ extra) where
  showsPrec a_ak9H ((:+) b1_ak9I b2_ak9J)
    = GHC.Show.showParen
        (a_ak9H >= 2)
        ((GHC.Base..)
           (GHC.Show.showsPrec 2 b1_ak9I)
           ((GHC.Base..)
              (GHC.Show.showString " :+ ") (GHC.Show.showsPrec 2 b2_ak9J)))

instance (GHC.Read.Read core, GHC.Read.Read extra) =>
         GHC.Read.Read (core :+ extra) where
  readPrec
    = GHC.Read.parens
        (Text.ParserCombinators.ReadPrec.prec
           1
           (do a1_ak9K <- Text.ParserCombinators.ReadPrec.step
                            GHC.Read.readPrec
               GHC.Read.expectP (Text.Read.Lex.Symbol ":+")
               a2_ak9L <- Text.ParserCombinators.ReadPrec.step GHC.Read.readPrec
               GHC.Base.return ((:+) a1_ak9K a2_ak9L)))
  readList = GHC.Read.readListDefault
  readListPrec = GHC.Read.readListPrecDefault
