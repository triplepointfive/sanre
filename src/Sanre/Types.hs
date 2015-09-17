module Sanre.Types where

import qualified Data.Map.Strict as Map

type Node = (String, [String])
type Tree = Map.Map String [String]

