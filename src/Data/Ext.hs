{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings  #-}
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
--------------------------------------------------------------------------------
module Data.Ext
  ( (:+)(..), core, extra, _core, _extra, ext
  ) where

-- import Data.Ext.Default
import Data.Ext.ZeroCost
