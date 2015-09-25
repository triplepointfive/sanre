-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Smelkov Ilya
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Walker over the directory and builder the dependency tree.
--
-----------------------------------------------------------------------------
module Sanre.FileTree (
  buildDirTree
) where

import qualified Data.Map.Strict as Map

import Language.Haskell.Meta
import Language.Haskell.Exts.Syntax
import System.Directory.Tree

import Sanre.Types

-- | Recursively goes through the directory and build a tree.
buildDirTree :: FilePath -> IO Tree
buildDirTree filePath = do
    (_ :/ at) <- readDirectory filePath
    return (foldDirTree Map.empty at)

-- | Folds a node, which could be eight a dir or file.
-- Doesn't fail, all errors are swallowed.
foldDirTree :: Tree -> DirTree String -> Tree
foldDirTree tree (File _ content) = case parseHsModule content of
    Right m -> addNode (moduleNode m) tree
    Left  _ -> tree -- Just for now
foldDirTree tree (Dir _ nodes) = foldl foldDirTree tree nodes
foldDirTree tree (Failed _ _)  = tree

-- | Adds a node to a tree.
addNode :: Node -> Tree -> Tree
addNode (key, val) = Map.insert key val

-- | Converts module struct into a node.
moduleNode :: Module -> Node
moduleNode (Module _ (ModuleName modName) _ _ _ imp _) =
    (modName, (\ (ModuleName x) -> x) <$> importModule <$> imp)
