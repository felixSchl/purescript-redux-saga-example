module Example.Screens.Dashboard (
    dashboard
  , dashboardClass
  , DashboardProps
  ) where

import Control.Monad.Eff (Eff)
import Data.Foldable (foldl, for_)
import Data.Lens (to)
import Data.Maybe (isJust)
import Data.Profunctor.Strong (first)
import Data.String as S
import Data.Tuple (uncurry)
import Prelude hiding (div)
import React (ReactElement, preventDefault)
import React as React
import React.DOM as DOM
import React.DOM.Props as Props
import React.Redux as Redux
import React.Spaces (SpaceM, element, renderIn, text, (!), (^))
import React.Spaces.DOM (code, div, h2, h3, h4, h5, input, label, small, span,
  form, button, code, i, table, thead, tbody, tr, th, td)
import Unsafe.Coerce (unsafeCoerce)

import Example.Types

dashboard :: String -> ReactElement
dashboard username = Redux.createElement dashboardClass { username } []

type DashboardProps = { username :: String }

dashboardClass :: ∀ props. Redux.ReduxReactClass GlobalState DashboardProps _
dashboardClass = Redux.createClass mapStateToProps $
  let spec = Redux.spec' render
   in spec { componentDidMount = componentDidMount }

  where
  mapStateToProps = to $ uncurry go
    where
    go { isLoadingDashboard, dashboardLoadingError, dashboardLoadedUsers }
       { username } = { isLoadingDashboard
                      , dashboardLoadingError
                      , username
                      , dashboardLoadedUsers
                      }

  componentDidMount :: Redux.ComponentDidMount _ _ _ (Eff _) _
  componentDidMount dispatch this = void do
    dispatch $ pure $ LoadDashboardRequest

  render :: Redux.Render _ _ _ (Eff _) _
  render dispatch this = render' <$> React.getProps this
    where
    render' { username
            , isLoadingDashboard
            , dashboardLoadingError
            , dashboardLoadedUsers
            }
      = renderIn DOM.div' do
        div $ do
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

