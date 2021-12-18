{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC
    -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
#-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ext
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
module Data.Ext.ZeroCost
  -- ( (:+), pattern (:+), core, extra, _core, _extra, ext
  -- ) where
  where

-- -- import Data.Singletons
-- import Data.Singletons.Prelude.Maybe
-- import Control.DeepSeq
import Control.Lens hiding ((.=))
-- import Data.Aeson
-- import Data.Aeson.Types (typeMismatch)
-- import Data.Biapplicative
-- import Data.Functor.Const
import Data.Bifoldable
-- import Data.Bifunctor.Apply
import Data.Bitraversable
-- import Data.Type.Equality
-- import Data.Proxy
-- import Data.Bifunctor
-- -- import Data.Default
-- import Data.Functor.Apply (liftF2)
-- import Data.Semigroup.Bifoldable
-- import Data.Semigroup.Bitraversable
-- import GHC.Generics
-- import Test.QuickCheck

-- import qualified GHC.Read
-- import qualified GHC.Show
-- import qualified GHC.Base
-- import qualified Text.ParserCombinators.ReadPrec
-- import qualified Text.Read.Lex

--------------------------------------------------------------------------------


data family Ext c (me :: Maybe *)

newtype instance Ext c Nothing = CoreOnly c
data instance Ext c (Just e) = DefaultExt c e


type family Payload m where
  Payload Nothing  = ()
  Payload (Just e) = e



class BitraverseImpl (me :: Maybe *) (me' :: Maybe *) where
  bitraverseImpl :: Applicative f
                 => (c -> f c')
                 -> (Payload me -> f (Payload me'))
                 -> Ext c me -> f (Ext c' me')

instance BitraverseImpl Nothing Nothing where
  bitraverseImpl f _ (CoreOnly c) = CoreOnly <$> f c
instance BitraverseImpl Nothing (Just e') where
  bitraverseImpl f g (CoreOnly c) = DefaultExt <$> f c <*> g ()
instance BitraverseImpl (Just e) Nothing where
  bitraverseImpl f g (DefaultExt c e) = (\c' _ -> CoreOnly c') <$> f c <*> g e
instance BitraverseImpl (Just e) (Just e') where
  bitraverseImpl f g (DefaultExt c e) = DefaultExt <$> f c <*> g e

class Construct me where
  construct :: c -> Payload me -> Ext c me
instance Construct Nothing where
  construct c _ = CoreOnly c
instance Construct (Just e) where
  construct = DefaultExt

-- newtype Compute me c r = Compute { run :: Ext c me -> r me }
type Compute me c r = Ext c me -> r me

class Select me where
  select :: Compute Nothing             c r
         -> Compute (Just (Payload me)) c r
         -> Ext c me -> r me
-- this is essentially a lens isn't it?

run = id

instance Select Nothing where
  select f _ x = run f x
instance Select (Just e) where
  select _ g x = run g x


type Compute' me c r = Compute me c (Const r)

newtype ThePayload (me :: Maybe *) = ThePayload { getPayload :: Payload me }

core' :: Select me => Lens (Ext c me) (Ext c' me) c c'
core' = lens _core setC
{-# SPECIALIZE INLINE core' :: Lens (Ext c Nothing)  (Ext c' Nothing)  c c' #-}
{-# SPECIALIZE INLINE core' :: Lens (Ext c (Just e)) (Ext c' (Just e)) c c' #-}


_core :: forall c me. Select me => Ext c me -> c
_core = getConst . select coreOnly def
  where
    coreOnly :: Compute' Nothing c c
    coreOnly = \(CoreOnly c) -> Const c

    def :: Compute' (Just e) c c
    def = \(DefaultExt c _) -> Const c

setC     :: forall c c' me. Select me => Ext c me -> c' -> Ext c' me
setC x c' = select coreOnly def $ x
  where
    coreOnly   :: Ext c Nothing -> Ext c' Nothing
    coreOnly _ = CoreOnly c'

    def (DefaultExt _ e) = DefaultExt c' e


_extra :: Select me => Ext c me -> Payload me
_extra = getPayload . select coreOnly def
  where
    coreOnly :: Compute Nothing c ThePayload
    coreOnly = const $ ThePayload ()

    def :: Compute (Just e) c ThePayload
    def = \(DefaultExt _ e) -> ThePayload e

-- setE      :: --forall me me' c. Select me' => Ext c me -> Payload me' -> Ext c me'

setE :: (Construct me', Select me) => Ext c me -> Payload me' -> Ext c me'
setE = construct . _core

extra' :: (Construct me', Select me)
       => Lens (Ext c me) (Ext c me') (Payload me) (Payload me')
extra' = lens _extra setE
{-# SPECIALIZE INLINE extra' :: Lens (Ext c Nothing)  (Ext c Nothing)   () () #-}
{-# SPECIALIZE INLINE extra' :: Lens (Ext c Nothing)  (Ext c (Just e')) () e' #-}
{-# SPECIALIZE INLINE extra' :: Lens (Ext c (Just e)) (Ext c Nothing)   e  () #-}
{-# SPECIALIZE INLINE extra' :: Lens (Ext c (Just e)) (Ext c (Just e')) e  e' #-}


-- biFoldMap' f g = bitraverse


-- bifoldMap'     :: (Monoid m, Select me) => (c -> m) -> (Payload me -> m) -> Ext c me -> m
-- bifoldMap' f g = getConst . select (\(CoreOnly c)     -> Const $ f c)
--                                    (\(DefaultExt c e) -> Const $ f c <> g e)
-- {-# SPECIALIZE INLINE bifoldMap' :: Monoid m => (c -> m) -> (() -> m) -> Ext c Nothing  -> m #-}
-- {-# SPECIALIZE INLINE bifoldMap' :: Monoid m => (c -> m) -> (e  -> m) -> Ext c (Just e) -> m #-}
-- this is subtelly wrong, since we do need to apply g to () to be correct.


-- newtype Unfolded me c e = MkUnfolded

-- newtype Ok me c e = MkOk (Ext c me)


-- instance Bitraversable (Ok me) where
--   bitraverse f g (MkExt x) = MkExt <$> bitraverseImpl f g x


-- -- type a :+ b = ()
-- data c :+ e where
--   MkExt :: (e ~ Payload me, Select me, Construct me) => Ext c me -> c :+ e

-- _core'           :: c :+ e -> c
-- _core' (MkExt x) = _core x

type family SelectM e = r | r -> e where
  SelectM () = Nothing
  SelectM e  = Just e


type family MaybeF e where
  MaybeF () = Nothing
  MaybeF e  = Just e

type Nothing' e = Nothing

type family EitherToMaybe e where
  EitherToMaybe (Left e) = Nothing
  EitherToMaybe (Right e) = Just e


-- newtype ExtF either (c :: *) (e :: *) = MkExtF (Ext c (EitherToMaybe (either e)))


-- instance Bitraversable (Eitherf either)


newtype ExtF maybe (c :: *) (e :: *) = MkExtF (Ext c (maybe e))

type c :+! e = ExtF Just c e


-- destruct   :: Select me => Ext c me -> (c, Payload me)
destruct   :: Select me => Ext c me -> (c, Payload me)
destruct x = (_core x, _extra x)

pattern (:+) :: (Payload me ~ e, Select me, Construct me) => c -> e -> Ext c me
pattern c :+ e <- (destruct -> ~(c,e))
  where
    c :+ e = construct c e




newtype c :+ e = MkExt { unExt :: Ext c (SelectM e) }






bitraverse'                :: forall f c c' e e'.
                              ( Applicative f
                              , BitraverseImpl (SelectM e) (SelectM e')
                              , e  ~ Payload (SelectM e)
                              , e' ~ Payload (SelectM e')
                              )
                           => (c -> f c')
                           -> (e -> f e')
                           -> c :+ e
                           -> f (c' :+ e')
bitraverse' f g (MkExt x) = MkExt <$> bitraverseImpl f g x

bimap'                :: forall c c' e e'.
                              ( BitraverseImpl (SelectM e) (SelectM e')
                              , e  ~ Payload (SelectM e)
                              , e' ~ Payload (SelectM e')
                              )
                           => (c -> c')
                           -> (e -> e')
                           -> c :+ e
                           -> c' :+ e'
bimap' f g = runIdentity . bitraverse' (Identity . f) (Identity . g)

first' f = bimap' f id

bifoldMap'     :: forall c e m.
                  (Monoid m
                  , BitraverseImpl (SelectM e) (SelectM e)
                  , e  ~ Payload (SelectM e)
               ) => (c -> m) -> (e -> m) -> c :+ e -> m
bifoldMap' f g = getConst . bitraverse' (Const @m @c . f) (Const @m @e . g)


test :: (BitraverseImpl (SelectM e) (SelectM e)
        , e  ~ Payload (SelectM e)) => Int :+ e -> Int :+ e
test = first' (+1)

test2 = test $ ext 5


instance Bifunctor     (:+) where
  bimap = bimapDefault
instance Bifoldable    (:+) where
  bifoldMap = bifoldMapDefault
instance Bitraversable (:+) where
  -- bitraverse :: forall f c c' e e'. Applicative f
  --            => (c -> f c')
  --            -> (e -> f e')
  --            -> c :+ e
  --            -> f (c' :+ e')
  -- bitraverse = bitraverse' @f @c @c' @e @e'


core :: Lens (c :+ e) (c' :+ e) c c'
core = undefined -- lens (\(MkExt x) -> _core x) undefined

extra = undefined

-- ext ::
ext :: c -> c :+ ()
ext = MkExt . CoreOnly
