module Example.Screens.Login (
    login
  , loginClass
  ) where

import Example.Types
import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Data.Foldable (foldl)
import Data.Maybe (isJust)
import Data.String as S
import React (ReactElement, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import React.Redux as Redux
import React.Spaces (SpaceM, element, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, h4, h5, input, label, small, span, form, button, code, i)
import Unsafe.Coerce (unsafeCoerce)

login :: ReactElement
login = Redux.createElement loginClass {} []

loginClass :: Redux.ConnectClass GlobalState {} _ Action
loginClass = Redux.connect mapStateToProps mapDispatchToProps {} klass

  where
  klass = React.createClass $
            React.spec { username: "admin", password: "password"} \this -> do
              render this
                <$> React.getProps this
                <*> React.readState this

  mapDispatchToProps dispatch _
    = { login: \username password ->
          void do
            dispatch $ LoginRequest { username, password }
      }

  mapStateToProps { isLoggingIn, loginError } _
    = { isLoggingIn, loginError }

  render this { isLoggingIn, loginError, login } state = renderIn DOM.div' do
    div
      ! Props.className "box"
      $ do
        h3
          ! Props.className "title is-5"
          $ text "Please login:"
        form
          ! Props.onSubmit (\event -> do
              void $ preventDefault event
              { username, password } <- React.readState this
              login username password
            )
          $ do
            div
              ! Props.className "field"
              $ do
                input
                  ! Props.value state.username
                  ! Props.placeholder "username"
                  ! Props.className "input"
                  ! Props._type "text"
                  ! Props.onChange (\event ->
                      let text = (unsafeCoerce event).target.value
                      in React.transformState this (_{ username = text })
                    )
            div
              ! Props.className "field"
              $ do
                input
                  ! Props.value state.password
                  ! Props.placeholder "password"
                  ! Props.className "input"
                  ! Props._type "password"
                  ! Props.onChange (\event ->
                      let text = (unsafeCoerce event).target.value
                      in React.transformState this (_{ password = text })
                    )
            div
              ! Props.className "field"
              $ do
                button
                  ! Props.value "login"
                  ! (Props.className $ "button is-primary "
                            <> if isLoggingIn
                                then "is-loading"
                                else ""
                    )
                  ! (Props.disabled $ foldl (||) false
                      [ isLoggingIn
                      , S.null $ S.trim state.username
                      , S.null $ S.trim state.password
                      ]
                    )
                  $ text "Submit"

                when (isJust loginError) do
                  div
                    ! Props.className "help is-danger"
                    $ text "Authentication failed. Please try again."
