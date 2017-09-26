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
   . Eff _ (Redux.Store Action GlobalState)
mkStore = Redux.createStore reducer initialState (middlewareEnhancer <<< reduxDevtoolsExtensionEnhancer')
  where
  initialState :: GlobalState
  initialState = { isLoggingIn: false
                 , loginError: Nothing
                 , loggedInAs: Nothing
                 , isLoadingDashboard: false
                 , dashboardLoadingError: Nothing
                 , dashboardLoadedUsers: []
                 }

  reduxDevtoolsExtensionEnhancer' :: Redux.Enhancer _ Action GlobalState
  reduxDevtoolsExtensionEnhancer' = Redux.fromEnhancerForeign reduxDevtoolsExtensionEnhancer

  middlewareEnhancer :: Redux.Enhancer _ Action GlobalState
  middlewareEnhancer = Redux.applyMiddleware [ sagaMiddleware saga ]

foreign import reduxDevtoolsExtensionEnhancer :: ∀ action state. Redux.EnhancerForeign action state
