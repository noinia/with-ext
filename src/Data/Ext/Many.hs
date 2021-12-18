{-# LANGUAGE TypeFamilyDependencies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ext.Many
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A pair-like data type to represent a 'core' type that has extra information
-- as well.
--
--------------------------------------------------------------------------------
module Data.Ext.Many where

import Control.DeepSeq
import Control.Lens
import Data.Coerce
import Data.Vinyl
import GHC.Generics (Generic)
import GHC.TypeLits
import Test.QuickCheck


data family Ext (core :: *) (extra :: Maybe *) :: *

newtype instance Ext core Nothing = Only core
data instance Ext core (Just extra) = !core :+ extra

type family core :+ extra where
  core :+ ()    = Ext core Nothing
  core :+ extra = Ext core (Just extra)


type WithExtra core extra = Ext core (Just extra)

instance Bifunctor WithExtra where
  bimap f g (c :+ e) = f c :+ g e



-- instance Bifunctor (:+) where

-- data family Ext core extra




-- newtype Many labels = ManyExtra (Rec Identity labels)


-- newtype instance Ext core ()            = Only core
-- data instance Ext core (Many labels) = Many !core (Many labels)




-- data family
