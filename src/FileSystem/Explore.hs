{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
module FileSystem.Explore where

import Control.Monad (forM_)
import qualified Control.Exception as Cexc
import qualified Data.Char as DC
import Data.List (isSuffixOf)
import qualified Data.Map as Mp
import Data.Text (Text)
-- import Data.List.Split (splitOn, splitOneOf)
import qualified Data.Vector as Vc
import qualified Data.Sequence as Seq
import qualified System.Directory.PathWalk as Wlk
import System.FilePath (joinPath, splitDirectories, makeRelative)
import qualified System.IO.Error as Serr

import Options.Runtime (RunOptions (..))
import FileSystem.Types


loadFolderTree :: (FilePath -> Bool) -> FilePath -> IO (Either String PathFiles)
loadFolderTree  filterFn rootPath = do
  -- DBG: putStrLn "@[loadFolderTree] starting."
  eiRez <- Cexc.try (Wlk.pathWalkAccumulate rootPath (filesAnalyser filterFn $ 1 + length rootPath)) :: IO (Either Serr.IOError PathFiles)
  case eiRez of
    Left exception -> pure . Left $ "@[loadFolderTree] err: " <> show exception
    Right rez -> pure $ Right rez


filesAnalyser :: (FilePath -> Bool) -> Int -> FilePath -> [FilePath] -> [[Char]] -> IO PathFiles
filesAnalyser filterFn prefixLength root dirs files =
  if ".git" `elem` splitDirectories root then
    pure Seq.empty
  else
    let
      items = foldl (\accum file ->
                      if filterFn file then
                        case itemMaker file of
                          Nothing -> accum
                          Just anItem -> accum <> [anItem]
                      else
                        accum
                      ) [] files
    in
    if length items > 0 then
      pure $ Seq.singleton (drop prefixLength root, items)
    else
      pure Seq.empty


itemMaker :: FilePath -> Maybe FileItem
itemMaker fileName
  | ".html" `isSuffixOf` fileName = Just $ KnownFile Html fileName
  | ".yaml" `isSuffixOf` fileName = Just $ KnownFile Yaml fileName
  | ".toml" `isSuffixOf` fileName = Just $ KnownFile Toml fileName
  | ".json" `isSuffixOf` fileName = Just $ KnownFile Json fileName
  | ".dant" `isSuffixOf` fileName = Just $ KnownFile DanTmpl fileName
  | ".hs" `isSuffixOf` fileName = Just $ KnownFile Haskell fileName
  | ".elm" `isSuffixOf` fileName = Just $ KnownFile Elm fileName
  | ".ts" `isSuffixOf` fileName = Just $ KnownFile Typescript fileName
  | ".js" `isSuffixOf` fileName = Just $ KnownFile Javascript fileName
  | ".tsx" `isSuffixOf` fileName = Just $ KnownFile TsReact fileName
  | ".jsx" `isSuffixOf` fileName = Just $ KnownFile JsReact fileName
  | ".md" `isSuffixOf` fileName = Just $ KnownFile Markdown fileName
  | ".org" `isSuffixOf` fileName = Just $ KnownFile EmacsOrg fileName
  | ".css" `isSuffixOf` fileName = Just $ KnownFile Css fileName
  | ".adoc" `isSuffixOf` fileName = Just $ KnownFile AsciiDoc fileName
  | ".pandoc" `isSuffixOf` fileName = Just $ KnownFile Pandoc fileName
  | ".rss" `isSuffixOf` fileName = Just $ KnownFile Rss fileName
  | ".xml" `isSuffixOf` fileName = Just $ KnownFile Xml fileName
  | ".txt" `isSuffixOf` fileName = Just $ KnownFile TxtTempl fileName
  | ".php" `isSuffixOf` fileName = Just $ KnownFile Php fileName
  | otherwise = Just $ MiscFile fileName

