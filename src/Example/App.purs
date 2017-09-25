module Example.App (main) where

import Prelude
import Redux.Saga

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Rec.Class (forever)
import Data.Foldable (any, foldl)
import Data.Functor.Contravariant (cmap)
import Data.Lens (to)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first, second)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import React (ReactElement, ReactProps, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props (className)
import React.DOM.Props as Props
import React.Redux as Redux
import Unsafe.Coerce (unsafeCoerce)

type GlobalState =
  { isLoggingIn :: Boolean
  , loginError :: Maybe Error
  , loggedInAs :: Maybe { username :: String }
  }

data Action
  = LoginRequest { username :: String, password :: String }
  | LoginFailure Error
  | LoginSuccess { username :: String }
  | LoadDashboardRequest
  | LoadDashboardSuccess
  | LoadDashboardFailure Error

saga :: ∀ eff. Saga (console :: CONSOLE | eff) Action GlobalState Unit
saga = void do
  fork loginFlow

  where
  loginFlow = forever do
    take case _ of
      LoginRequest { username, password } -> pure do
        liftAff $ Console.log $ "Logging in as " <> show username
        liftAff $ delay $ 1000.0 # Milliseconds
        if username == "admin" && password == "password"
          then do
            put $ LoginSuccess { username }
            void postLoginFlow
          else put $ LoginFailure $ error "Authentication failed"
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
      _ -> state

type LocalState = { username :: String, password :: String }

appClass :: ∀ props. Redux.ReduxReactClass' _ _
appClass = Redux.createClass' id $ Redux.spec' render

  where
  render :: Redux.Render _ _ _ (Eff _) _
  render dispatch this = do
    DOM.div [ Props.className "container" ] <<< pure <$> do
      render' <$> React.getProps this

    where
    render' :: GlobalState -> ReactElement
    render' { loggedInAs: Just { username } } = dashboard username
    render' _ = loginScreen

dashboard :: String -> ReactElement
dashboard username = Redux.createElement dashboardClass { username } []

type DashboardProps = { username :: String }
dashboardClass :: ∀ props. Redux.ReduxReactClass props DashboardProps DashboardProps
dashboardClass = Redux.createClass second $
  let spec = Redux.spec' render
   in spec { componentDidMount = componentDidMount }

  where
  componentDidMount :: Redux.ComponentDidMount _ _ _ (Eff _) _
  componentDidMount dispatch this = void do
    dispatch $ pure $ LoadDashboardRequest

  render :: Redux.Render _ _ _ (Eff _) _
  render dispatch this = do
    DOM.div [ Props.className "container" ] <<< pure <$> do
      render' <$> React.getProps this

    where
    render' { username }
      = DOM.div []
          [ DOM.h3 [ Props.className "title is-3 has-text-centered" ]
              [ DOM.text $ "Welcome back, " <> username <> "!" ]
          ]

loginScreen :: ReactElement
loginScreen = Redux.createElement loginScreenClass [] []

loginScreenClass :: ∀ props. Redux.ReduxReactClass GlobalState props GlobalState
loginScreenClass = Redux.createClass first spec

  where
  spec :: Redux.Spec _ _ _ (Eff _) _
  spec = Redux.spec (\_ _ -> pure { username: "admin", password: "password"}) render

  render dispatch this =
    render' <$> React.getProps this
            <*> React.readState this

    where
    render' { isLoggingIn, loginError } state =
      DOM.form
        [ Props.className "box"
        , Props.onSubmit $ \event -> do
            void $ preventDefault event
            { username, password } <- React.readState this
            void $ dispatch $ pure $ LoginRequest { username, password }
        ] $
        [ DOM.h3 [ Props.className "title is-5" ]
            [ DOM.text "Please login:" ]
        , DOM.div [ Props.className "field" ] [
            DOM.input [ Props.value state.username
                      , Props.placeholder "username"
                      , Props.className "input"
                      , Props._type "text"
                      , Props.onChange \event ->
                          let text = (unsafeCoerce event).target.value
                           in React.transformState this (_{ username = text })
                      ] []
          ]
        , DOM.div [ Props.className "field" ] [
            DOM.input [ Props.value state.password
                      , Props.placeholder "password"
                      , Props.className "input"
                      , Props._type "password"
                      , Props.onChange \event ->
                          let text = (unsafeCoerce event).target.value
                           in React.transformState this (_{ password = text })
                      ] []
          ]
        , DOM.div [ Props.className "field" ] $ [
            DOM.button [ Props.value "login"
                       , Props.className $ "button is-primary "
                          <> if isLoggingIn
                              then "is-loading"
                              else ""
                       , Props.disabled $ foldl (||) false
                          [ isLoggingIn
                          , S.null $ S.trim state.username
                          , S.null $ S.trim state.password
                          ]
                      ] [ DOM.text "Submit" ]
            ] <> case loginError of
                  Nothing -> []
                  Just _ -> pure do
                    DOM.div [ Props.className "help is-danger" ]
                      [ DOM.text "Authentication failed. Please try again." ]
        ]

main :: Eff _ ReactElement
main = do
  store <- mkStore
  pure $ Redux.createProviderElement store appClass

foreign import reduxDevtoolsExtensionEnhancer :: forall action state. Redux.EnhancerForeign action state
