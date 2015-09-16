module Lib
    ( someFunc
    ) where

import Language.Haskell.Meta
import Language.Haskell.Exts.Syntax

someFunc :: IO ()
someFunc = putStrLn "someFunc"

listFileImports :: FilePath -> IO [String]
listFileImports filePath = do
  a <- parseHsModule <$> readFile filePath
  case a of
    Right m -> return (moduleImports m)
    Left  s -> putStrLn s >> return []

moduleImports :: Module -> [String]
moduleImports (Module _ _ _ _ _ imp _) =
    (\ (ModuleName x) -> x) <$> importModule <$> imp
