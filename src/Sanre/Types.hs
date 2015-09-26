{-# LANGUAGE DeriveDataTypeable  #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Smelkov Ilya
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common types with wide usage across the program.
--
-----------------------------------------------------------------------------
module Sanre.Types where

import Data.Data
import qualified Data.Map.Strict as Map

-- | A module and it's imports.
type Node = (String, [String])

-- | Represents the whole graph, contains the whole module names.
type Tree = Map.Map String [String]

-- | List of colors to be able to key it in as command line arg.
data Color = Blue
           | Black
           | Green
           deriving  (Show, Data, Typeable)

-- | Structure with all possible configurations, including tree building
-- and it's representation.
data Config = Config
    { directory :: FilePath
    , external  :: Bool
    , color     :: Color
    , uniq      :: Bool
    , fontColor :: Color
    } deriving (Show, Data, Typeable)
