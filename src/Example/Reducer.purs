module Example.Reducer where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Example.Types (Action(..), GlobalState(..))
import React.Redux as Redux

reducer :: Redux.Reducer Action GlobalState
reducer = wrap $ flip go
  where
  go state = case _ of
    LoginRequest _ ->
      state { isLoggingIn = true
            , loginError = Nothing
            }
    LoginFailure e ->
      state { isLoggingIn = false
            , loginError = Just e
            }
    LoginSuccess { username } ->
      state { isLoggingIn = false
            , loggedInAs = Just { username }
            , loginError = Nothing
            }
    LoadDashboardRequest ->
      state { isLoadingDashboard = true
            , dashboardLoadingError = Nothing
            }
    LoadDashboardSuccess { users } ->
      state { isLoadingDashboard = false
            , dashboardLoadingError = Nothing
            , dashboardLoadedUsers = users
            }
    LoadDashboardFailure error ->
      state { isLoadingDashboard = false
            , dashboardLoadingError = Just error
            }
    LogoutRequest ->
      state { isLoggingIn = false
            , loginError = Nothing
            , loggedInAs = Nothing
            }
    _ -> state
