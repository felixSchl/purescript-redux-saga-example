module Example.Types where

import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))

import Data.Maybe (Maybe)
import Control.Monad.Eff.Exception (Error)

type GlobalState =
  { isLoggingIn :: Boolean
  , loginError :: Maybe Error
  , loggedInAs :: Maybe { username :: String }
  , isLoadingDashboard :: Boolean
  , dashboardLoadingError :: Maybe Error
  , dashboardLoadedUsers :: Array User
  }

data Action
  = LoginRequest { username :: String, password :: String }
  | LoginFailure Error
  | LoginSuccess { username :: String }
  | LoadDashboardRequest
  | LoadDashboardFailure Error
  | LoadDashboardSuccess { users :: Array User }

newtype User = User { id :: String, username :: String, email :: String }

instance decodeJsonUser :: DecodeJson User where
  decodeJson j = decodeJson j >>= \m -> do
    User <$> do
      { id: _, username: _, email: _ }
        <$> m .? "id"
        <*> m .? "username"
        <*> m .? "email"
