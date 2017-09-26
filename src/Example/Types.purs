module Example.Types where

import Data.Maybe (Maybe)
import Control.Monad.Eff.Exception (Error)

type GlobalState =
  { isLoggingIn :: Boolean
  , loginError :: Maybe Error
  , loggedInAs :: Maybe { username :: String }
  , isLoadingDashboard :: Boolean
  , dashboardLoadingError :: Maybe Error
  }

data Action
  = LoginRequest { username :: String, password :: String }
  | LoginFailure Error
  | LoginSuccess { username :: String }
  | LoadDashboardRequest
  | LoadDashboardFailure Error
  | LoadDashboardSuccess

type User = { id :: String, username :: String, email :: String }

