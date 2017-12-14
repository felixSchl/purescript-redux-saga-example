module Example.App (main) where

import Prelude hiding (div)
import Redux.Saga

import Control.Monad.Aff (delay, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (either, Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Example.Screens.Dashboard (dashboard)
import Example.Screens.Login (login)
import Example.Store (mkStore)
import Example.Types (Action(..), User(User), GlobalState)
import Network.HTTP.Affjax as Affjax
import React (ReactElement, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import React.Redux as Redux
import React.Spaces (SpaceM, element, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, h4, h5, input, label, small, span, form, button, code, i, a)

appClass :: Redux.ConnectClass GlobalState {} _ Action
appClass = Redux.connect mapStateToProps mapDispatchToProps {} klass

  where
  klass = React.createClass $ React.spec {} render

  mapDispatchToProps _ _ = {}
  mapStateToProps { loggedInAs } _ = { loggedInAs }

  render this = render' <$> React.getProps this
    where
    render' { loggedInAs } = renderIn DOM.div' do
      div
        ! Props.className "container"
        $ do
          div ! Props.className "title is-6"
            $ do
                text "purescript-redux-saga demo - "
                a ! Props.href "https://github.com/felixschl/purescript-redux-saga"
                  $ text "https://github.com/felixschl/purescript-redux-saga"
          case loggedInAs of
            Nothing -> element login
            Just { username } -> element $ dashboard username

main :: Eff _ ReactElement
main = do
  store <- mkStore
  pure $ Redux.createProviderElement store [
    Redux.createElement_ appClass []
  ]
