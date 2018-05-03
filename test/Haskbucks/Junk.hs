{-# LANGUAGE OverloadedStrings #-}

module Haskbucks.Junk
( withPgFromEnv
) where
import qualified System.Posix.Env.ByteString as Env
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Aeson as JSON
import           Data.Typeable (Typeable)
import Control.Monad.Trans.Resource
import Data.Pool as Pool
import Haskbucks.Event
import Test.Hspec
import qualified Database.PostgreSQL.Simple as Pg
import Control.Exception (bracket)

withPgFromEnv :: (JSON.FromJSON ev, JSON.ToJSON ev, Typeable ev) =>
                       (EventLog ev (ResourceT IO) -> IO ()) -> IO ()
withPgFromEnv f = do
  pgUrl <- Env.getEnv $  Char8.pack "PG_URL"
  case pgUrl of
    Nothing -> do pendingWith "No $PG_URL specified"
    Just url -> do
     withPgPool url $ \pool -> do
      withPgLock pool $ do
        dropPgEvents pool
        withPgEvents pool f


withPgLock :: Pool Pg.Connection -> IO a -> IO a
withPgLock pool action = do
  bracket aquireLock releaseLock $ \_ -> action
  where
  aquireLock :: IO (Pg.Connection, LocalPool Pg.Connection)
  aquireLock = do
    (c, lpool) <- Pool.takeResource pool
    Pg.begin c
    _ <- Pg.query_ c "select pg_advisory_xact_lock(42);" :: IO [Pg.Only ()]
    return (c, lpool)

  releaseLock :: (Pg.Connection, LocalPool Pg.Connection) -> IO ()
  releaseLock (c, lpool) = do
    Pg.rollback c
    Pool.putResource lpool c
