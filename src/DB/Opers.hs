{-# LANGUAGE QuasiQuotes #-}

module DB.Opers where

import qualified Data.ByteString as Bs
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)

import Hasql.Session (Session, statement)
import Hasql.Pool (Pool, use)
import qualified Hasql.TH as TH


getVersion :: Pool -> Text -> IO (Either String Int32)
getVersion dbPool version = do
  rezA <- use dbPool $ locateVersion version
  case rezA of
    Left err -> pure . Left $ "@[getVersion] locateVersion err: " <> show err
    Right mbUid ->
      case mbUid of
        Just uid -> pure $ Right uid
        Nothing -> do
          rezB <- use dbPool $ insertVersion version
          case rezB of
            Left err -> pure . Left $ "@[getVersion] insertVersion err: " <> show err
            Right uid -> pure $ Right uid


getFolder :: Pool -> Int32 -> Text -> IO (Either String Int32)
getFolder dbPool versionID path = do
  rezA <- use dbPool $ locateFolder versionID path
  case rezA of
    Left err -> pure . Left $ "@[getFolder] locateFolder err: " <> show err
    Right mbUid -> case mbUid of
      Just uid -> pure $ Right uid
      Nothing -> do
        rezB <- use dbPool $ insertFolder versionID path
        case rezB of
          Left err -> pure . Left $ "@[getFolder] insertFolder err: " <> show err
          Right uid -> pure $ Right uid


getFile :: Pool -> Int32 -> Text -> IO (Either String Int32)
getFile dbPool folderID path = do
  rezA <- use dbPool $ locateFile folderID path
  case rezA of
    Left err -> pure . Left $ "@[getFile] locateFile err: " <> show err
    Right mbUid -> case mbUid of
      Just uid -> pure $ Right uid
      Nothing -> do
        rezB <- use dbPool $ insertFile folderID path
        case rezB of
          Left err -> pure . Left $ "@[getFile] insertFile err: " <> show err
          Right uid -> pure $ Right uid


addAST :: Pool -> Int32 -> Bs.ByteString -> IO (Either String ())
addAST dbPool fileID ast = do
  rezA <- use dbPool $ insertAST fileID ast
  case rezA of
    Left err -> pure . Left $ "@[addAST] insertAST err: " <> show err
    Right _ -> pure $ Right ()


addConstants :: Pool -> Int32 -> Bs.ByteString -> IO (Either String ())
addConstants dbPool fileID constants = do
  rezA <- use dbPool $ insertConstants fileID constants
  case rezA of
    Left err -> pure . Left $ "@[addConstants] insertConstants err: " <> show err
    Right _ -> pure $ Right ()


addError :: Pool -> Int32 -> Text -> Maybe NominalDiffTime -> IO (Either String ())
addError dbPool fileID message procTime = do
  rezA <- use dbPool $ insertError fileID message procTime
  case rezA of
    Left err -> pure . Left $ "@[addError] insertError err: " <> show err
    Right _ -> pure $ Right ()

-- *** SQL ***
locateVersion :: Text -> Session (Maybe Int32)
locateVersion version =
  statement version [TH.maybeStatement|
    select uid::int4 from WPVersion where label = $1::text
  |]


insertVersion :: Text -> Session Int32
insertVersion version =
  statement version [TH.singletonStatement|
    insert into WPVersion (label) values ($1::text) returning uid::int4
  |]


locateFolder :: Int32 -> Text -> Session (Maybe Int32)
locateFolder versionID path =
  statement (versionID, path) [TH.maybeStatement|
    select uid::int4 from Folder where versionRef = $1::int4 and path = $2::text
  |]


insertFolder :: Int32 -> Text -> Session Int32
insertFolder versionID path =
  statement (versionID, path) [TH.singletonStatement|
    insert into Folder (versionRef, path) values ($1::int4, $2::text) returning uid::int4
  |]


locateFile :: Int32 -> Text -> Session (Maybe Int32)
locateFile folderID path =
  statement (folderID, path) [TH.maybeStatement|
    select uid::int4 from File where folderRef = $1::int4 and path = $2::text
  |]


insertFile :: Int32 -> Text -> Session Int32
insertFile folderID path =
  statement (folderID, path) [TH.singletonStatement|
    insert into File (folderRef, path) values ($1::int4, $2::text) returning uid::int4
  |]


insertAST :: Int32 -> Bs.ByteString -> Session ()
insertAST fileID ast =
  statement (fileID, ast) [TH.resultlessStatement|
    insert into AST (fileRef, value) values ($1::int4, $2::bytea)
  |]


insertConstants :: Int32 -> Bs.ByteString -> Session ()
insertConstants fileID constants =
  statement (fileID, constants) [TH.resultlessStatement|
    insert into Constant (fileRef, value) values ($1::int4, $2::bytea)
  |]


insertError :: Int32 -> Text -> Maybe NominalDiffTime -> Session ()
insertError fileID message mbProcTime =
  statement (fileID, message, realToFrac <$> mbProcTime) [TH.resultlessStatement|
    insert into Error (fileRef, message, procTime) values ($1::int4, $2::text, $3::float4?)
  |]
