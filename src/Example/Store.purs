module Example.Store where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Prelude hiding (div)
import React.Redux as Redux
import Redux.Saga (sagaMiddleware)

import Example.Saga (saga)
import Example.Reducer (reducer)
import Example.Types (GlobalState, Action)

mkStore
  :: ∀ eff
   . Eff _ (Redux.ReduxStore _ GlobalState Action)
mkStore = Redux.createStore reducer initialState middlewareEnhancer
  where
  initialState :: GlobalState
  initialState = { isLoggingIn: false
                 , loginError: Nothing
                 , loggedInAs: Just { username: "admin" }
                 , isLoadingDashboard: false
                 , dashboardLoadingError: Nothing
                 , dashboardLoadedUsers: []
                 }

  middlewareEnhancer :: Redux.ReduxStoreEnhancer _ GlobalState Action
  middlewareEnhancer = Redux.applyMiddleware [ sagaMiddleware saga ]
