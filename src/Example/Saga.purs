module Example.Saga where

import Debug.Trace
import Control.Monad.Aff (delay, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (either, Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Example.Types (GlobalState, Action(..), User(User))
import Network.HTTP.Affjax as Affjax
import Prelude hiding (div)
import Redux.Saga

saga :: Saga Unit GlobalState Action Unit
saga = void do
  -- log all actions as they come through
  void $ fork $ forever $ take $ pure <<< traceAnyA

  -- fork the eternal login flow
  fork loginFlow

  where
  loginFlow = forever do
    take case _ of
      LoginRequest { username, password } -> pure $ void $ fork do
        liftAff $ Console.log $ "Logging in as " <> show username
        liftAff $ delay $ 1000.0 # Milliseconds
        if username == "admin" && password == "password"
          then do
            void $ fork $ postLoginFlow username
            put $ LoginSuccess { username }
          else put $ LoginFailure $ error "Authentication failed"
      _ -> Nothing

  postLoginFlow username = forever do
    take case _ of
      LoadDashboardRequest -> pure $ void $ fork do
        liftAff $ Console.log $ "Loading dashboard..."
        result <- liftAff $ attempt do
          liftAff $ delay $ 1000.0 # Milliseconds -- pretend this took some time
          { response } <- liftAff $ Affjax.get "https://jsonplaceholder.typicode.com/users"
          case decodeJson response of
            Right users -> pure users
            Left err -> throwError $ error err
        case result of
          Left error -> put $ LoadDashboardFailure error
          Right users -> put $ LoadDashboardSuccess { users }
      _ -> Nothing
