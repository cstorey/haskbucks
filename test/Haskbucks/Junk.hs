module Haskbucks.Junk
( withPgFromEnv
) where
import qualified System.Posix.Env.ByteString as Env
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Aeson as JSON
import           Data.Typeable (Typeable)
import Control.Monad.Trans.Resource
import Haskbucks.Event
import Test.Hspec

withPgFromEnv :: (JSON.FromJSON ev, JSON.ToJSON ev, Typeable ev) =>
                       (EventLog ev (ResourceT IO) -> IO ()) -> IO ()
withPgFromEnv f = do
  pgUrl <- Env.getEnv $  Char8.pack "PG_URL"
  case pgUrl of
    Nothing -> do pendingWith "No $PG_URL specified"
    Just url -> withPgEvents url f
