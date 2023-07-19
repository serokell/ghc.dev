{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.DH (generateDhPage) where

import Data.Time.Clock
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Clay as C
import Clay ((?), (-:))
import qualified Data.ByteString.Lazy as BS.L

import Pages.Common
import Pages.Graph

roadmap :: Roadmap
roadmap = runRoadmapBuilder mdo
  setGoalLevels [
      [openEvaluator, closedEvaluatior],
      [typelevelCleanup],
      [typeSyntaxInTerms],
      [invisibleBindersInTypes],
      [standaloneKindSignatures]
    ]

  dh <- mkGoal
    Goalpost {
      title = "Dependent Haskell",
      subtitle = "dependent types in GHC",
      completed = False
    } [dependentQuantifiaction, functionPromotion, typelevelCleanup]

  dependentQuantifiaction <- mkGoal
    Goalpost {
      title = "Dependent retained quantifiers",
      subtitle = "types that refer to terms",
      completed = False
    } [dependentProducts, dependentSums]

  dependentProducts <- mkGoal
    Goalpost {
      title = "Dependent products",
      subtitle = "Π-types",
      completed = False
    } [dependentCore, visibleForall]

  dependentSums <- mkGoal
    Goalpost {
      title = "Dependent sums",
      subtitle = "Σ-types",
      completed = False
    } [dependentCore]

  dependentCore <- mkGoal
    Goalpost {
      title = "Dependent Core language",
      subtitle = "formalism and implementation",
      completed = False
    } []

  functionPromotion <- mkGoal
    Goalpost {
      title = "Function promotion",
      subtitle = "promote term-level functions",
      completed = False
    } [closedEvaluatior, openEvaluator]

  closedEvaluatior <- mkGoal
    Goalpost {
      title = "Closed term evaluator",
      subtitle = "using bytecode or native code",
      completed = False
    } []

  openEvaluator <- mkGoal
    Goalpost {
      title = "Open term evaluator",
      subtitle = "normalization by evaluation",
      completed = False
    } []

  typelevelCleanup <- mkGoal
    Goalpost {
      title = "Clean up the language",
      subtitle = "preparations for dependent types",
      completed = False
    }
    [
      standaloneKindSignatures,
      invisibleBindersInTypes,
      invisibleBindersInTerms,
      visibleForall,
      typeSyntaxInTerms
    ]

  standaloneKindSignatures <- mkGoal
    Goalpost {
      title = "Standalone kind signatures",
      subtitle = "replacement for CUSKs",
      completed = True
    } []

  invisibleBindersInTypes <- mkGoal
    Goalpost {
      title = "Invisible binders in types",
      subtitle = "@k-binders in type declarations",
      completed = True
    } []

  invisibleBindersInTerms <- mkGoal
    Goalpost {
      title = "Invisible binders in terms",
      subtitle = "@a-binders in lambdas",
      completed = False
    } []

  typeSyntaxInTerms <- mkGoal
    Goalpost {
      title = "Syntactic unification",
      subtitle = "type syntax in terms",
      completed = False
    } [visibleForall]

  visibleForall <- mkGoal
    Goalpost {
      title = "Visible forall",
      subtitle = "in types of terms",
      completed = False
    } [typeSyntaxInTerms]

  pure dh

generateDhPage :: UTCTime -> IO H.Html
generateDhPage time = do
  roadmapSvg <- extractToSvg roadmap
  return (dhPage time roadmapSvg)

dhPage :: UTCTime -> BS.L.ByteString -> H.Html
dhPage time roadmapSvg = do
  (H.docTypeHtml ! A.lang "en") do
    H.head do
      H.meta ! A.charset "utf-8"
      H.meta
        ! A.name "viewport"
        ! A.content "width=device-width, initial-scale=1.0"
      H.title "GHC Development"
      styleEmbedCss dhStyle
    H.body do
      (H.div ! A.class_ "header") do
        headerHtml "dependent types roadmap"
      (H.div ! A.class_ "roadmap") do
        H.unsafeLazyByteString roadmapSvg
      (H.div ! A.class_ "footer") do
        footerHtml time

dhStyle :: C.Css
dhStyle = do
  commonStyle
  ".roadmap" ? do
    C.display C.flex
    C.flexDirection C.column
    C.alignItems C.center
    "border" -: "1px solid #333333"
  ".roadmap svg" ? do
    C.padding (C.px 20) (C.px 20) (C.px 20) (C.px 20)
    -- C.border C.solid (C.px 1) C.magenta
    C.width (C.pct 90)
    C.minWidth (C.px 600)
    C.maxWidth (C.px 1200)