module Example.Screens.Dashboard (
    dashboard
  , dashboardClass
  , DashboardProps
  ) where

import Example.Types
import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Data.Foldable (foldl, for_)
import Data.Maybe (isJust)
import Data.String as S
import Data.Tuple (uncurry)
import React (ReactElement, getProps, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import React.Redux as Redux
import React.Spaces (SpaceM, element, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, h4, h5, input, label, small, span, form, button, code, i, table, thead, tbody, tr, th, td)
import Unsafe.Coerce (unsafeCoerce)

dashboard :: String -> ReactElement
dashboard username = Redux.createElement dashboardClass { username } []

type DashboardProps = { username :: String }

dashboardClass :: Redux.ConnectClass GlobalState DashboardProps _ Action -- GlobalState DashboardProps _ Action
dashboardClass = Redux.connect mapStateToProps mapDispatchToProps {} klass

  where

  klass = React.createClass $
            let spec = React.spec unit \this -> do
                          render <$> React.getProps this
             in spec { componentDidMount = componentDidMount }

  mapDispatchToProps dispatch _
    = { logout: void $ dispatch LogoutRequest
      , loadDashboard: void $ dispatch LoadDashboardRequest
      }

  mapStateToProps { isLoadingDashboard
                  , dashboardLoadingError
                  , dashboardLoadedUsers
                  } { username }
    = { isLoadingDashboard
      , dashboardLoadingError
      , dashboardLoadedUsers
      , username
      }

  componentDidMount this = do
    { loadDashboard } <- React.getProps this
    loadDashboard

  render { username
         , isLoadingDashboard
         , dashboardLoadingError
         , dashboardLoadedUsers
         , logout
         }
      = renderIn DOM.div' do
        div $ do
          button
            ! Props.className "button is-pulled-right is-primary"
            ! Props.onClick (const logout)
            $ do
               text $ "log out"
          h3
            ! Props.className "title is-3 has-text-centered"
            $ do
                text $ "Welcome back, " <> username <> "!"
          when (isLoadingDashboard) do
            div
              ! Props.className "has-text-centered"
              $ do
                div ! Props.className "fa fa-spinner fa-spin is-size-6" $ pure unit
          when (isJust dashboardLoadingError) do
            div
              ! Props.className "has-text-centered has-text-danger"
              $ do
                text "An error occurred loading the dashboard."

          unless isLoadingDashboard do
            h4 ! Props.className "title is-4" $ text "Recent users"
            table
              ! Props.className "table is-fullwidth is-striped"
              $ do
                thead do
                  tr do
                    th $ text "id"
                    th $ text "email"
                    th $ text "username"
                tbody do
                  for_ dashboardLoadedUsers \(User { id, email, username }) -> do
                    tr do
                      td $ text $ show id
                      td $ text email
                      td $ text username

