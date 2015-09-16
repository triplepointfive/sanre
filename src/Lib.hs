module Lib
    ( someFunc
    ) where

import qualified Data.Map.Strict as Map

import Language.Haskell.Meta
import Language.Haskell.Exts.Syntax
import System.Directory.Tree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Node = (String, [String])
type Tree = Map.Map String [String]

buildDirTree :: FilePath -> IO Tree
buildDirTree filePath = do
    (_ :/ at) <- readDirectory filePath
    return (foldDirTree Map.empty at)

foldDirTree :: Tree -> DirTree String -> Tree
foldDirTree tree (File _ content) = case parseHsModule content of
    Right m -> addNode (moduleNode m) tree
    Left  s -> tree -- Just for now
foldDirTree tree (Dir _ nodes) = foldl foldDirTree tree nodes
foldDirTree tree (Failed _ _)  = tree

addNode :: Node -> Tree -> Tree
addNode (key, val) = Map.insert key val

moduleNode :: Module -> Node
moduleNode (Module _ (ModuleName modName) _ _ _ imp _) =
    (modName, (\ (ModuleName x) -> x) <$> importModule <$> imp)
