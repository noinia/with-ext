{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC
    -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
    -ddump-to-file
#-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ext.Zero
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A pair-like data type to represent a 'core' type that has extra information
-- as well.
--
-- implementation uses a data family and some type trickery to
-- (hopefully) get GHC to make this into a zero cost thing.
--
--------------------------------------------------------------------------------
module Data.Ext.Zero
  ( (:+)((:+), Core), core, extra, _core, _extra, ext

  , Ext(..), ExtRep(..), Select(..), select2', Construct(..)
  , core', extra'
  , bimap', bitraverse', bifoldMap'', withUnit
  ) where


import           Control.DeepSeq
import           Control.Lens hiding ((.=))
import           Data.Proxy
-- import Data.Bifunctor
-- import Data.Biapplicative
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Void
import qualified GHC.Read
import qualified Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex
-- import           GHC.Generics
import           Test.QuickCheck
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Coerce (coerce)

--------------------------------------------------------------------------------

data family Ext (rep :: ExtRep) c e :: *

data ExtRep = NoExt | WithExt

newtype instance Ext NoExt   c e = CoreOnly c
data    instance Ext WithExt c e = DefaultExt !c !e


-- | Class that can select the right, optimzied data type
class Select (rep :: ExtRep) where
  select :: (Ext NoExt   c e -> r NoExt)
         -> (Ext WithExt c e -> r WithExt)
         -> Ext rep c e -> r rep

  select2 :: (Ext NoExt   c e -> Ext NoExt   c' e' -> r NoExt)
          -> (Ext WithExt c e -> Ext WithExt c' e' -> r WithExt)
          -> Ext rep c e -> Ext rep c' e' -> r rep


instance Select NoExt where
  select f _ = f
  select2 f _ = f
instance Select WithExt where
  select _ g = g
  select2 _ g = g

-- | Flipped version of select2
select2'         :: Select rep => Ext rep c e -> Ext rep c' e'
                 -> (Ext NoExt c e   -> Ext NoExt   c' e' -> r NoExt)
                 -> (Ext WithExt c e -> Ext WithExt c' e' -> r WithExt)
                 -> r rep
select2' a b f g = select2 f g a b


select2Diff'             :: (Select rep, Select rep') => Ext rep c e -> Ext rep' c' e'
                         -> (Ext NoExt c e   -> Ext NoExt   c' e' -> r)
                         -> (Ext NoExt c e   -> Ext WithExt c' e' -> r)
                         -> (Ext WithExt c e -> Ext NoExt   c' e' -> r)
                         -> (Ext WithExt c e -> Ext WithExt c' e' -> r)
                         -> r
select2Diff' a b f g h i = getConst $ select co def a
  where
    co  a' = coerce $ select (\b' -> Const $ f a' b') (\b' -> Const $ g a' b') b
    def a' = coerce $ select (\b' -> Const $ h a' b') (\b' -> Const $ i a' b') b


-- | Class that represents that we cna construct an optimized representation.
class Construct rep where
  construct :: c -> e -> Ext rep c e
instance Construct NoExt where
  construct c _ = CoreOnly c
instance Construct WithExt where
  construct = DefaultExt

-- | Access the core data.
_core' ::forall rep c e. Select rep => Ext rep c e -> c
_core' = getConst . select (\(CoreOnly c)     -> Const c)
                           (\(DefaultExt c _) -> Const c)
{-# SPECIALIZE INLINE _core' :: Ext NoExt   c e -> c  #-}
{-# SPECIALIZE INLINE _core' :: Ext WithExt c e -> c  #-}

-- -- | Get only the core of an Ext
-- pattern Core'  :: forall rep c e. Select rep => c -> Ext rep c e
-- pattern Core' c <- (_core' -> c)

-- | Acess the extra data value, if one exists. Otherwise this throws
-- an error.
_extra' :: Select rep => Ext rep c e -> e
_extra' = getConst
        . select (\_                -> Const $ error "Ext NoExt does not store an extra!")
                 (\(DefaultExt _ e) -> Const e)
{-# SPECIALIZE INLINE _extra' :: Ext NoExt   c e -> e  #-}
{-# SPECIALIZE INLINE _extra' :: Ext WithExt c e -> e  #-}

-- | Lens to access the core
core' :: forall rep c c' e. (Select rep, Construct rep) => Lens (Ext rep c e) (Ext rep c' e) c c'
core' = lens _core' _set
  where
    _set      :: Ext rep c e -> c' -> Ext rep c' e
    _set x c' = construct c' (_extra' x)
    -- Note that the use of extra is safe, even if rep is a 'NoExt,
    -- since in that case' we immediately throw away the error again
    -- in 'construct' without looking at it.
{-# SPECIALIZE INLINE core' :: Lens (Ext NoExt   c e) (Ext NoExt   c' e) c c' #-}
{-# SPECIALIZE INLINE core' :: Lens (Ext WithExt c e) (Ext WithExt c' e) c c' #-}

-- | Lens to access the extra value, if there exists one.
extra' :: Construct rep => Lens (Ext WithExt c e) (Ext rep c e') e e'
extra' = lens (\(DefaultExt _ e) -> e) (\x -> construct (_core' x))
{-# SPECIALIZE INLINE extra' :: Lens (Ext WithExt c e) (Ext NoExt   c e') e e' #-}
{-# SPECIALIZE INLINE extra' :: Lens (Ext WithExt c e) (Ext WithExt c e') e e' #-}

--------------------------------------------------------------------------------

instance (Select rep, Construct rep) => Bifunctor (Ext rep) where
  {-# SPECIALIZE instance Bifunctor (Ext NoExt  ) #-}
  {-# SPECIALIZE instance Bifunctor (Ext WithExt) #-}
  bimap = bimap'

instance Select rep => Bifoldable (Ext rep) where
  {-# SPECIALIZE instance Bifoldable (Ext NoExt  ) #-}
  {-# SPECIALIZE instance Bifoldable (Ext WithExt) #-}
  bifoldMap = bifoldMap'

instance (Select rep, Construct rep) => Bitraversable (Ext rep) where
  {-# SPECIALIZE instance Bitraversable (Ext NoExt  ) #-}
  {-# SPECIALIZE instance Bitraversable (Ext WithExt) #-}
  bitraverse = bitraverse'

-- | Implementation of bimap that also allows changing representation.
--
--
-- note that we invent an non-existing element to apply 'g' on in the
-- CoreOnly scenario. So the function 'g' better not inspect this
-- value.
bimap'       :: (Select rep, Construct rep')
             => (c -> c') -> (e -> e') -> Ext rep c e -> Ext rep' c' e'
bimap' f g x = construct (f . _core' $ x) (g . _extra' $ x)

-- | Implemetation of foldmap.
--
-- note that we invent an non-existing element to apply 'g' on in the
-- CoreOnly scenario. So the function 'g' better not inspect this
-- value.
bifoldMap'     :: (Select rep, Monoid m) => (c -> m) -> (e -> m) -> Ext rep c e -> m
bifoldMap' f g = getConst . select (\(CoreOnly c)     -> Const $ f c <> g err)
                                   (\(DefaultExt c e) -> Const $ f c <> g e)
  where
    err = error "Data.Ext.Zero: bifoldMap', applying g to an non-existing element!"

-- | A version of bifoldmap that does not invent non-existing elements
-- when applying them to a CoreOnly.
bifoldMap''     :: (Select rep, Monoid m) => (c -> m) -> (e -> m) -> Ext rep c e -> m
bifoldMap'' f g = getConst . select (\(CoreOnly c)     -> Const $ f c)
                                    (\(DefaultExt c e) -> Const $ f c <> g e)


-- | Helper data type to implement bitraverse'
newtype Wrap f c e rep' rep = Wrap { unWrap :: f (Ext rep' c e) }

-- | Implementation of bitraverse. Note that we can change type if we
-- have to.
--
-- note that we invent an non-existing element to apply 'g' on in the
-- CoreOnly scenario. So the function 'g' better not inspect this value.
bitraverse'     :: forall rep rep' f c c' e e'. (Select rep, Construct rep', Applicative f)
                => (c -> f c') -> (e -> f e') -> Ext rep c e -> f (Ext rep' c' e')
bitraverse' f g = unWrap . select coreOnly def
  where
    coreOnly              :: Ext NoExt c e -> Wrap f c' e' rep' NoExt
    coreOnly (CoreOnly c) = Wrap $ construct <$> f c <*> g err
    err = error "Data.Ext.Zero: bitraverse', applying g to an non-existing element!"

    def                  :: Ext WithExt c e -> Wrap f c' e' rep' WithExt
    def (DefaultExt c e) = Wrap $ construct <$> f c <*> g e

--------------------------------------------------------------------------------

-- | Given a function that we can run on both values of type 'e' and
-- on (), run the function on the values stored in the Ext; if the Ext
-- has an e value in it, we use it in the function. If we are not
-- storing an 'e' we invent a value of type () instead.
withUnit   :: forall proxy constraint rep c e r.
              (constraint e, constraint (), Select rep)
            => proxy constraint
           -> (forall e'. constraint e' => c -> e' -> r)
           -> Ext rep c e
           -> r
withUnit _ f = getConst . select (\(CoreOnly c)     -> Const $ f c ())
                                 (\(DefaultExt c e) -> Const $ f c e)


instance (Show c, Show e, Select rep) => Show (Ext rep c e) where
  showsPrec p = withUnit (Proxy @Show) showsPrec'

  -- showsPrec p = getConst . select (\(CoreOnly c)     -> Const $ showsPrec' c ())
  --                                 (\(DefaultExt c e) -> Const $ showsPrec' c e)
    where
      showsPrec'     :: (Show c, Show e') => c -> e' -> ShowS
      showsPrec' c e = showParen (p >= 2) (showsPrec 2 c . showString " :+ " . showsPrec 2 e)

instance (Read c, Read e, Construct rep) => Read (Ext rep c e) where
  readPrec =
    GHC.Read.parens
        (Text.ParserCombinators.ReadPrec.prec
           1
           (do c <- Text.ParserCombinators.ReadPrec.step
                            GHC.Read.readPrec
               GHC.Read.expectP (Text.Read.Lex.Symbol ":+")
               e <- Text.ParserCombinators.ReadPrec.step GHC.Read.readPrec
               pure $ construct c e))
  readList     = GHC.Read.readListDefault
  readListPrec = GHC.Read.readListPrecDefault

instance (Eq c, Eq e, Select rep) => Eq (Ext rep c e) where
  {-# SPECIALIZE instance (Eq c, Eq e) => Eq (Ext NoExt   c e) #-}
  {-# SPECIALIZE instance (Eq c, Eq e) => Eq (Ext WithExt c e) #-}
  a == b = getConst $ select2' a b
    (\(CoreOnly c)     (CoreOnly c')      -> Const $ c == c')
    (\(DefaultExt c e) (DefaultExt c' e') -> Const $ c == c' && e == e')

instance (Ord c, Ord e, Select rep) => Ord (Ext rep c e) where
  {-# SPECIALIZE instance (Ord c, Ord e) => Ord (Ext NoExt   c e) #-}
  {-# SPECIALIZE instance (Ord c, Ord e) => Ord (Ext WithExt c e) #-}
  a `compare` b = getConst $ select2' a b
    (\(CoreOnly c)     (CoreOnly c')      -> Const $ c `compare` c')
    (\(DefaultExt c e) (DefaultExt c' e') -> Const $ c `compare` c' <> e `compare` e')



newtype WrapI c e rep = WrapI { unWrapI :: Ext rep c e }

instance (Semigroup c, Semigroup e, Select rep) => Semigroup (Ext rep c e) where
  {-# SPECIALIZE instance (Semigroup c, Semigroup e) => Semigroup (Ext NoExt   c e) #-}
  {-# SPECIALIZE instance (Semigroup c, Semigroup e) => Semigroup (Ext WithExt c e) #-}
  a <> b = unWrapI $ select2' a b
           (\(CoreOnly c)     (CoreOnly c')      -> WrapI $ CoreOnly $ c <> c')
           (\(DefaultExt c e) (DefaultExt c' e') -> WrapI $ DefaultExt (c <> c') (e <> e'))


-- instance Generic (Ext rep c e) where
--   type instance Rep (Ext rep c e)
--     = D1
--       ('MetaData
--          ":+" "Data.Ext.Zero" "with-ext" 'False)
--       (C1
--          ('MetaCons ":+" ('InfixI 'RightAssociative 1) 'False)
--          (S1
--             ('MetaSel
--                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--             (Rec0 c)
--           :*: S1
--                 ('MetaSel
--                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                 (Rec0 e)))

--   from x = select (\(CoreOnly ))


  -- x_aka8
  --   = GHC.Generics.M1
  --       (case x_aka8 of {
  --          (:+) g1_aka9 g2_akaa
  --            -> GHC.Generics.M1
  --                 ((GHC.Generics.:*:)
  --                    (GHC.Generics.M1 (GHC.Generics.K1 g1_aka9))
  --                    (GHC.Generics.M1 (GHC.Generics.K1 g2_akaa))) })
  -- to (GHC.Generics.M1 x_akab)
  --   = case x_akab of {
  --       (GHC.Generics.M1 ((GHC.Generics.:*:) (GHC.Generics.M1 (GHC.Generics.K1 g1_akac))
  --                                            (GHC.Generics.M1 (GHC.Generics.K1 g2_akad))))
  --         -> (:+) g1_akac g2_akad }

instance (NFData c, NFData e, Select rep) => NFData (Ext rep c e) where
  {-# SPECIALIZE instance (NFData c, NFData e) => NFData (Ext NoExt   c e) #-}
  {-# SPECIALIZE instance (NFData c, NFData e) => NFData (Ext WithExt c e) #-}
  rnf = getConst . select (\(CoreOnly c)     -> Const $ rnf c)
                          (\(DefaultExt c e) -> Const $ rnf (c,e))


instance (ToJSON core, ToJSON extra, Select rep) => ToJSON (Ext rep core extra) where
  toJSON     = withUnit (Proxy @ToJSON) $ \c e -> object ["core" .= c, "extra" .= e]
  toEncoding = withUnit (Proxy @ToJSON) $ \c e -> pairs  ("core" .= c <> "extra" .= e)

instance (FromJSON core, FromJSON extra, Construct rep) => FromJSON (Ext rep core extra) where
  -- parseJSON = fmap (\(c,e) -> c :+ e) . parseJSON
  parseJSON (Object v) = construct <$> v .: "core" <*> v .: "extra"
  parseJSON invalid    = typeMismatch "Ext (:+)" invalid

instance (Arbitrary c, Arbitrary e, Construct rep) => Arbitrary (Ext rep c e) where
  arbitrary = construct <$> arbitrary <*> arbitrary

-- instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
--   arbitrary = (:+) <$> arbitrary <*> arbitrary



-- instance Generic (core :+ extra) where
--   type instance Rep (core :+ extra)
--     = D1
--       ('MetaData
--          ":+" "Data.Ext" "hgeometry-combinatorial" 'False)
--       (C1
--          ('MetaCons ":+" ('InfixI 'RightAssociative 1) 'False)
--          (S1
--             ('MetaSel
--                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--             (Rec0 core)
--           :*: S1
--                 ('MetaSel
--                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--                 (Rec0 extra)))

--   from x_aka8
--     = GHC.Generics.M1
--         (case x_aka8 of {
--            (:+) g1_aka9 g2_akaa
--              -> GHC.Generics.M1
--                   ((GHC.Generics.:*:)
--                      (GHC.Generics.M1 (GHC.Generics.K1 g1_aka9))
--                      (GHC.Generics.M1 (GHC.Generics.K1 g2_akaa))) })
--   to (GHC.Generics.M1 x_akab)
--     = case x_akab of {
--         (GHC.Generics.M1 ((GHC.Generics.:*:) (GHC.Generics.M1 (GHC.Generics.K1 g1_akac))
--                                              (GHC.Generics.M1 (GHC.Generics.K1 g2_akad))))
--           -> (:+) g1_akac g2_akad }


--------------------------------------------------------------------------------

type family Payload rep e where
  Payload NoExt   () = ()
  Payload WithExt e  = e

data family ThePayload e (rep :: ExtRep)
newtype instance ThePayload e NoExt   = MkPayloadNoExt   Void
newtype instance ThePayload e WithExt = MkPayloadWithExt e


_extra'' :: forall rep c e. Select rep => Ext rep c e -> ThePayload e rep
_extra'' = select (\_                -> MkPayloadNoExt $ error "Ext NoExt does not store an extra!")
                  (\(DefaultExt _ e) -> MkPayloadWithExt e)











-- instance (Select rep, Construct rep) => Bifunctor (Ext rep) where
--   bimap f g x = x&core'  %~ f
--                  &extra' %~ g



--------------------------------------------------------------------------------
-- Hide the particular rep from the user.

-- | a core value c together with an e'
data c :+ e where
  MkExt :: forall rep c e. (Construct rep, Select rep) => Ext rep c e -> c :+ e


-- | Retrieve only the core of the value.
pattern Core :: c -> c :+ e
pattern Core c <- (_core -> c)


pattern (:+)  :: c -> e -> c :+ e
pattern c :+ e <- (destruct -> ~(c,e))
  where
    c :+ e = MkExt $ construct @WithExt c e
-- FIXME: Same here!
-- FIXME: This is partial!!!!


-- | get the only the core value out of an ext
_core           :: (c :+ e) -> c
_core (MkExt x) = _core' x

-- | get the extra value out of an ext.
--
-- note that this extra value may not exist!, hence this function is partial.
_extra           :: (c :+ e) -> e
_extra (MkExt x) = _extra' x

-- | Lens to access the core.
core :: Lens (c :+ e) (c' :+ e) c c'
core = lens _core (\(MkExt x) c' -> MkExt $ x&core' .~ c')

-- | Lens to access the extra
extra :: forall c e e'. Lens (c :+ e) (c :+ e') e e'
extra = lens _extra (\(MkExt x) e' -> MkExt $ construct @WithExt (_core' x) e')
-- extra = lens _extra (\(MkExt x) e' -> MkExt $ construct @(OptimalRep e') (_core' x) e')
-- FIXME: we want the optimal rep here. But then GHC does not see that optimalrep also
-- satisfies Select :(

type family OptimalRep e where
  OptimalRep () = NoExt
  OptimalRep e  = WithExt

instance (Show c, Show e) => Show (c :+ e) where
  showsPrec p (MkExt x) = showsPrec p x

-- instance (Read c, Read e) => Read (c :+ e) where
--   readPrec p = do x <- GHC.Read.readPrec @(Ext WithExt c e) p
--                   pure $ MkExt x


instance (Eq c, Eq e) => Eq (c :+ e) where
  (MkExt a) == (MkExt b) = select2Diff' a b (==) (\_ _ -> False) (\_ _ -> False) (==)


-- instance (Ord c, Ord e) => Ord (c :+ e) where
--   (MkExt a) `compare` (MkExt b) = a `compare` b









-- | Constructs an optimal representation for a CoreOnly
ext   :: c -> c :+ ()
ext c = MkExt $ CoreOnly c

-- instance Bifunctor (:+) where
--   bimap f g (MkExt x) = MkExt (bimap' f g x)


-- newtype c :+ e = MkExt (forall rep. (Construct rep, Select rep) => Ext rep c e)

destruct          :: c :+ e -> (c, e)
destruct (MkExt x) = (_core' x, _extra' x)


-- select' :: (Ext NoExt   c e -> r NoExt)
--         -> (Ext WithExt c e -> r WithExt)
--         -> c :+ e -> r















-- instance HasExtra (Ext NoExt c ()) (Ext NoExt c ()) () () where
--   extra = lens (const ()) (\x _ -> x)

-- instance HasExtra (Ext NoExt c ()) (Ext WithExt c e') () e' where
--   extra = lens (const ()) (\(CoreOnly c) e' -> DefaultExt c e')

-- instance HasExtra (Ext WithExt c e) (Ext NoExt c ())  e () where
--   extra = lens (\(DefaultExt _ e) -> e) (\(DefaultExt c _) _ -> CoreOnly c)

-- instance HasExtra (Ext WithExt c e) (Ext WithExt c e') e e' where
--   extra = lens (\(DefaultExt _ e) -> e) (\(DefaultExt c _) e' -> DefaultExt c e')


-- instance HasCore (Ext NoExt c e) (Ext NoExt c' e) c c' where
--   core = lens (\(CoreOnly c) -> c) (\_ -> CoreOnly)
-- instance HasCore (Ext WithExt c e) (Ext WithExt c' e) c c' where
--   core = lens (\(DefaultExt c _) -> c) (\(DefaultExt _ e) c -> DefaultExt c e)



-- _core' :: Ext rep c e -> c
-- _core' =



  -- (const ()) (\x _ -> x)

-- newtype c :+ e = MkExt (forall c' e' s t. (HasCore s t c c', HasExtra s t e e') )


--------------------------------------------------------------------------------

test1 = DefaultExt 5 'c'

test2 = CoreOnly 10


testRead :: Ext NoExt Int ()
testRead = read "10 :+ ()"

testRead2 :: Ext WithExt Int Int
testRead2 = read "10 :+ 5"
