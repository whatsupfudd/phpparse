module Options.Runtime (defaultRun, RunOptions (..), PgDbConfig (..), defaultPgDbConf) where
-- import Data.Int (Int)

import Data.Text (Text)

import DB.Connect (PgDbConfig (..), defaultPgDbConf)


data RunOptions = RunOptions {
    debug :: Int
    , pgDbConf :: PgDbConfig
    , version :: Text
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , pgDbConf = defaultPgDbConf
    , version = "6.2.1"
  }
