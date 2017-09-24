module Example.App (main) where

import Prelude
import Redux.Saga

import Control.Monad.Aff (delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Lens (Lens', Prism', lens, prism', to, view)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first, second)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd, uncurry)
import React (preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props (className)
import React.DOM.Props as Props
import React.Redux (REDUX)
import React.Redux as Redux

type GlobalState =
  { isLoggingIn :: Boolean
  , loginError :: Maybe Error
  , loggedInAs :: Maybe { username :: String }
  }

data Action
  = LoginRequest { username :: String, password :: String }
  | LoginFailure Error
  | LoginSuccess { username :: String }

saga :: ∀ eff. Saga (console :: CONSOLE | eff) Action GlobalState Unit
saga = do
  void $ takeEvery case _ of
    LoginRequest { username, password } -> pure do
      liftAff $ Console.log $ "Logging in as " <> show username
      liftAff $ delay $ 1000.0 # Milliseconds
      pure unit
    _ -> Nothing

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
      LoginRequest _ -> state { isLoggingIn = true }
      LoginFailure _ -> state { isLoggingIn = false }
      LoginSuccess _ -> state { isLoggingIn = false }
      _ -> state

type LocalState = { username :: String, password :: String }

appClass :: ∀ props. Redux.ReduxReactClass GlobalState props GlobalState
appClass = Redux.createClass first spec

  where
  spec :: Redux.Spec _ _ _ (Eff _) _
  spec = Redux.spec (\_ _ -> pure { username: "", password: ""}) render

  render dispatch this = render' <$> React.getProps this
                                 <*> React.readState this
    where
    render' :: GlobalState -> LocalState -> React.ReactElement
    render' { loggedInAs: Just { username } } _
      = DOM.div []
                [ DOM.text $ "Welcome back, " <> username ]
    render' { isLoggingIn } state =
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
                      ] []
          ]
        , DOM.div [ Props.className "field" ] [
            DOM.input [ Props.value state.password
                      , Props.placeholder "password"
                      , Props.className "input"
                      , Props._type "password"
                      ] []
          ]
        , DOM.div [ Props.className "field" ] $ [
            DOM.input [ Props.value "login"
                      , Props._type "submit"
                      , Props.className "button is-primary"
                      , Props.disabled isLoggingIn
                      ] []
          ] <> if isLoggingIn
                  then [ DOM.span [ Props.className "fa fa-spinner fa-spin" ] [] ]
                  else []
        ]
main :: Eff _ React.ReactElement
main = do
  store <- mkStore
  pure $ Redux.createProviderElement store appClass

foreign import reduxDevtoolsExtensionEnhancer :: forall action state. Redux.EnhancerForeign action state
