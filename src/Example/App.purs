module Example.App (main) where

import Debug.Trace
import Control.Monad.Aff (delay, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Core (Json)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Example.Screens.Login (login)
import Example.Screens.Dashboard (dashboard)
import Network.HTTP.Affjax as Affjax
import Prelude hiding (div)
import React (ReactElement, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import React.Redux as Redux
import React.Spaces (SpaceM, element, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, h4, h5, input, label, small, span, form, button, code, i)
import Redux.Saga

import Example.Types (GlobalState, Action(..))

saga :: ∀ eff. Saga (console :: CONSOLE | eff) Action GlobalState Unit
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
            put $ LoginSuccess { username }
            void postLoginFlow
          else put $ LoginFailure $ error "Authentication failed"

      LoadDashboardRequest -> pure $ void $ fork do
        liftAff $ Console.log $ "Loading dashboard..."
        result <- liftAff $ attempt do
          { response: (r :: Json) } <- liftAff $ Affjax.get "https://jsonplaceholder.typicode.com/users"
          traceAnyA r
          pure unit -- TODO
        case result of
          Right result -> put $ LoadDashboardSuccess
          Left error -> put $ LoadDashboardFailure error
      _ -> Nothing

  postLoginFlow = do
    pure unit

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
                 }

  reduxDevtoolsExtensionEnhancer' :: Redux.Enhancer _ Action GlobalState
  reduxDevtoolsExtensionEnhancer' = Redux.fromEnhancerForeign reduxDevtoolsExtensionEnhancer

  middlewareEnhancer :: Redux.Enhancer _ Action GlobalState
  middlewareEnhancer = Redux.applyMiddleware [ sagaMiddleware saga ]

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
      LoadDashboardSuccess ->
        state { isLoadingDashboard = false
              , dashboardLoadingError = Nothing
              }
      LoadDashboardFailure error ->
        state { isLoadingDashboard = false
              , dashboardLoadingError = Just error
              }
      _ -> state

type LocalState = { username :: String, password :: String }

appClass :: ∀ props. Redux.ReduxReactClass' _ _
appClass = Redux.createClass' id $ Redux.spec' render

  where
  render :: Redux.Render _ _ _ (Eff _) _
  render dispatch this = render' <$> React.getProps this
    where
    render' :: GlobalState -> ReactElement
    render' { loggedInAs } = renderIn DOM.div' do
      div
        ! Props.className "container"
        $ do
          div
            ! Props.className "title is-6"
            $ text "purescript-redux-saga demo"
          case loggedInAs of
            Nothing -> element login
            Just { username } -> element $ dashboard username

main :: Eff _ ReactElement
main = do
  store <- mkStore
  pure $ Redux.createProviderElement store appClass

foreign import reduxDevtoolsExtensionEnhancer :: forall action state. Redux.EnhancerForeign action state
