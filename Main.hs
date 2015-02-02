import Paths_auth_helper

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import qualified Data.Text as T
import qualified Data.Text.IO as T


data User = User
    { email :: Text
    }
  deriving (Generic, Show)

instance FromJSON User
instance ToJSON User


emptyUsers :: IO (TVar [User])
emptyUsers =
    newTVarIO []

getUsers :: MonadIO m => TVar [User] -> m [User]
getUsers users =
    liftIO $ readTVarIO users

postUser :: MonadIO m => TVar [User] -> User -> m [User]
postUser users user =
    liftIO $ do
      T.putStrLn $ email user
      atomically $ do
        oldUsers <- readTVar users
        let newUsers = user : oldUsers
        writeTVar users newUsers
        return newUsers


type AuthAPI =
    "users" :> Get [User] :<|>
    "users" :> ReqBody User :> Post [User]

authAPI :: Proxy AuthAPI
authAPI =
    Proxy

authServer :: TVar [User] -> Server AuthAPI
authServer users =
    getUsers users :<|>
    postUser users


start :: Application -> IO ()
start app = do
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        sets = setPort port defaultSettings
    keyFile <- getDataFileName "server.key"
    crtFile <- getDataFileName "server.crt"
    hasKey <- doesFileExist keyFile
    hasCrt <- doesFileExist crtFile
    if hasKey && hasCrt
      then do
        putStrLn $ "Running with TLS on " ++ show port
        let tlsOpts = tlsSettings keyFile crtFile
        runTLS tlsOpts sets app
      else do
        putStrLn $ "Running without TLS on " ++ show port
        runSettings sets app


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    users <- emptyUsers
    start $ serve authAPI $ authServer users
