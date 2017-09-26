module Example.Reducer where

import Prelude
import React.Redux as Redux
import Data.Maybe (Maybe(..))
import Example.Types (Action(..), GlobalState(..))

reducer :: Redux.Reducer Action GlobalState
reducer = flip go
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
    _ -> state
