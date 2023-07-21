{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Pages.DH (generateDhPage) where

import Data.Time.Clock
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Clay as C
import Clay ((?), (-:))
import qualified Data.ByteString.Lazy as BS.L
import Data.Foldable (traverse_)

import Pages.Common
import Pages.Graph

roadmap :: Roadmap
roadmap = runRoadmapBuilder mdo
  setGoalLevels [
      [dh],
      [dependentQuantification, universalPromotion],
      [typelevelLambdas],
      [openEvaluator, closedEvaluatior],
      [typelevelCleanup],
      [invisibleBindersInFunctions],
      [invisibleBindersInTypes, invisibleBindersInConstructors],
      [standaloneKindSignatures],
      [syntacticUnification],
      [punFreeCode],
      [noListTuplePuns],
      [whitespaceSensitiveOperators],
      [noStarIsType, noKindVars],
      [typeInType]
    ]

  dh <- mkGoal
    Goalpost {
      title = "Dependent Haskell",
      subtitle = "dependent types in GHC",
      description = H.p "TODO: description",
      completed = False
    } [dependentQuantification, universalPromotion, typelevelCleanup]

  dependentQuantification <- mkGoal
    Goalpost {
      title = "Dependent retained quantifiers",
      subtitle = "types that refer to terms",
      description = H.p "TODO: description",
      completed = False
    } [dependentProducts, dependentSums]

  dependentProducts <- mkGoal
    Goalpost {
      title = "Dependent products",
      subtitle = "Π-types",
      description = H.p "TODO: description",
      completed = False
    } [dependentCore, visibleForall]

  dependentSums <- mkGoal
    Goalpost {
      title = "Dependent sums",
      subtitle = "Σ-types",
      description = H.p "TODO: description",
      completed = False
    } [dependentCore, existentialQuantification]

  dependentCore <- mkGoal
    Goalpost {
      title = "Dependent Core language",
      subtitle = "formalism and implementation",
      description = H.p "TODO: description",
      completed = False
    } []

  universalPromotion <- mkGoal
    Goalpost {
      title = "Universal promotion",
      subtitle = "promote all terms",
      description = H.p "TODO: description",
      completed = False
    }
    [
      functionPromotion,
      literalPromotion,
      unboxedPromotion,
      constraintPromotion
    ]

  literalPromotion <- mkGoal
    Goalpost {
      title = "Promote literals",
      subtitle = "numeric, char, string",
      description = H.p "TODO: description",
      completed = False
    } [charPromotion, natPromotion, constrainedTypeFamilies]

  charPromotion <- mkGoal
    Goalpost {
      title = "Promote Char",
      subtitle = "and character literals",
      description = H.p "TODO: description",
      completed = True
    } []

  natPromotion <- mkGoal
    Goalpost {
      title = "Promote Natural",
      subtitle = "unify it with Nat",
      description = H.p "TODO: description",
      completed = True
    } []

  unboxedPromotion <- mkGoal
    Goalpost {
      title = "Promote unboxed types",
      subtitle = "RuntimeRep ≠ LiftedRep",
      description = H.p "TODO: description",
      completed = False
    } [homogeneousEquality]

  homogeneousEquality <- mkGoal
    Goalpost {
      title = "Homogeneous equality",
      subtitle = "instead of heterogeneous",
      description = H.p "TODO: description",
      completed = False
    } []

  constraintPromotion <- mkGoal
    Goalpost {
      title = "Promote classes",
      subtitle = "constraints and dictionaries",
      description = H.p "TODO: description",
      completed = False
    } [functionPromotion, constrainedTypeFamilies]

  functionPromotion <- mkGoal
    Goalpost {
      title = "Function promotion",
      subtitle = "promote term-level functions",
      description = H.p "TODO: description",
      completed = False
    } [closedEvaluatior, openEvaluator, typelevelLambdas]

  closedEvaluatior <- mkGoal
    Goalpost {
      title = "Closed term evaluator",
      subtitle = "using bytecode or native code",
      description = H.p "TODO: description",
      completed = False
    } []

  openEvaluator <- mkGoal
    Goalpost {
      title = "Open term evaluator",
      subtitle = "normalization by evaluation",
      description = H.p "TODO: description",
      completed = False
    } []

  typelevelLambdas <- mkGoal
    Goalpost {
      title = "Type-level lambdas",
      subtitle = "first-class functions in types",
      description = H.p "TODO: description",
      completed = False
    } [dependentCore, openEvaluator]

  typelevelCleanup <- mkGoal
    Goalpost {
      title = "Clean up the language",
      subtitle = "preparations for dependent types",
      description = H.p "TODO: description",
      completed = False
    }
    [
      invisibleBinders,
      visibleForall,
      existentialQuantification,
      unsaturatedTypeFamilies,
      constrainedTypeFamilies,
      noKindVars,
      noCusks
    ]

  existentialQuantification <- mkGoal
    Goalpost {
      title = "Existential quantification",
      subtitle = "first-class exists quantifier",
      description = H.p "TODO: description",
      completed = False
    } []

  unsaturatedTypeFamilies <- mkGoal
    Goalpost {
      title = "Unsaturated type families",
      subtitle = "partial application in types",
      description = H.p "TODO: description",
      completed = False
    } [modifiersSyntax]

  constrainedTypeFamilies <- mkGoal
    Goalpost {
      title = "Constrained type families",
      subtitle = "constraints in kinds",
      description = H.p "TODO: description",
      completed = False
    } []

  modifiersSyntax <- mkGoal
    Goalpost {
      title = "Modifiers syntax",
      subtitle = "generalized modifiers",
      description = H.p "TODO: description",
      completed = False
    } []

  standaloneKindSignatures <- mkGoal
    Goalpost {
      title = "Standalone kind signatures",
      subtitle = "replacement for CUSKs",
      description = H.p "TODO: description",
      completed = True
    } []

  invisibleBinders <- mkGoal
    Goalpost {
      title = "Invisible type variable binders",
      subtitle = "in types and terms",
      description = H.p "TODO: description",
      completed = False
    }
    [
      invisibleBindersInTypes,
      invisibleBindersInFunctions,
      invisibleBindersInConstructors
    ]

  invisibleBindersInTypes <- mkGoal
    Goalpost {
      title = "Invisible binders in types",
      subtitle = "@k-binders in type declarations",
      description = H.p "TODO: description",
      completed = True
    } [standaloneKindSignatures]

  invisibleBindersInFunctions <- mkGoal
    Goalpost {
      title = "Invisible binders in functions",
      subtitle = "@a-binders in lambdas",
      description = H.p "TODO: description",
      completed = False
    } []

  invisibleBindersInConstructors <- mkGoal
    Goalpost {
      title = "Invisible binders in constructors",
      subtitle = "@a-binders in constructor patterns",
      description = H.p "TODO: description",
      completed = True
    } []

  syntacticUnification <- mkGoal
    Goalpost {
      title = "Syntactic unification",
      subtitle = "type syntax in terms",
      description = H.p "TODO: description",
      completed = False
    }
    [
      termVariableCapture,
      noStarIsType,
      forallKeyword,
      tildeOperator,
      punFreeCode
    ]

  punFreeCode <- mkGoal
    Goalpost {
      title = "Support pun-free code",
      subtitle = "to resolve ambiguities",
      description = H.p "TODO: description",
      completed = False
    }
    [
      namespaceSpecifiedImports,
      noListTuplePuns,
      punWarnings
    ]

  namespaceSpecifiedImports <- mkGoal
    Goalpost {
      title = "Namespace-specified imports",
      subtitle = "filter imported names",
      description = H.p "TODO: description",
      completed = False
    } []

  noListTuplePuns <- mkGoal
    Goalpost {
      title = "No list or tuple puns",
      subtitle = "remove ambiguous built-in syntax",
      description = H.p "TODO: description",
      completed = False
    } []

  punWarnings <- mkGoal
    Goalpost {
      title = "Pun warnings",
      subtitle = "warn on puns and pun bindings",
      description = H.p "TODO: description",
      completed = False
    } [namespaceSpecifiedImports]

  visibleForall <- mkGoal
    Goalpost {
      title = "Visible forall",
      subtitle = "in types of terms",
      description = H.p "TODO: description",
      completed = False
    } [visCoercions, embedTypes, nestedGadtForalls, syntacticUnification]

  visCoercions <- mkGoal
    Goalpost {
      title = "Visibility coercions",
      subtitle = "in the Core language",
      description = H.p "TODO: description",
      completed = True
    } []

  nestedGadtForalls <- mkGoal
    Goalpost {
      title = "Nested quantification in GADTs",
      subtitle = "interleave foralls in constructors",
      description = H.p "TODO: description",
      completed = False
    } []

  embedTypes <- mkGoal
    Goalpost {
      title = "Embed types",
      subtitle = "into expressions",
      description = H.p "TODO: description",
      completed = False
    } []

  termVariableCapture <- mkGoal
    Goalpost {
      title = "Term variable capture",
      subtitle = "mention term variables in types",
      description = H.p "TODO: description",
      completed = False
    } []

  noStarIsType <- mkGoal
    Goalpost {
      title = "No star kind syntax",
      subtitle = "resolve conflict with operators",
      description = H.p "TODO: description",
      completed = True
    } [typeInType]

  forallKeyword <- mkGoal
    Goalpost {
      title = "The forall keyword",
      subtitle = "at the term level",
      description = H.p "TODO: description",
      completed = False
    } []

  tildeOperator <- mkGoal
    Goalpost {
      title = "The tilde operator",
      subtitle = "for equality constraints",
      description = H.p "TODO: description",
      completed = True
    } [whitespaceSensitiveOperators]

  whitespaceSensitiveOperators <- mkGoal
    Goalpost {
      title = "Whitespace-sensitive operators",
      subtitle = "prefix vs infix occurrences",
      description = H.p "TODO: description",
      completed = True
    } []

  noKindVars <- mkGoal
    Goalpost {
      title = "Unify type and kind variables",
      subtitle = "consistent quantification rules",
      description = H.p "TODO: description",
      completed = True
    } [typeInType]

  noCusks <- mkGoal
    Goalpost {
      title = "Deprecate CUSKs",
      subtitle = "complete user-supplied kinds",
      description = H.p "TODO: description",
      completed = False
    } [invisibleBindersInTypes]

  typeInType <- mkGoal
    Goalpost {
      title = "Unify types and kinds",
      subtitle = "with the Type :: Type axiom",
      description = H.p "TODO: description",
      completed = True
    } []

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
      (H.div ! A.class_ "goals") do
        traverse_ goalHtml (roadmap.goals)
      (H.div ! A.class_ "footer") do
        footerHtml time

dhStyle :: C.Css
dhStyle = do
  commonStyle
  ".roadmap" ? do
    C.display C.flex
    C.flexDirection C.column
    C.alignItems C.center
    C.overflowX C.auto
  ".roadmap svg" ? do
    C.sym C.padding (C.px 20)
    -- C.border C.solid (C.px 1) C.magenta
    C.width (C.pct 90)
    C.minWidth (C.px 600)
    C.maxWidth (C.px 1200)
  ".goals" ? do
    C.borderTop (C.px 1) C.solid (C.rgb 0x33 0x33 0x33)
    C.sym C.padding (C.px 30)
    C.display C.grid
    C.justifyContent C.center
    "grid-template-columns" -: "repeat(auto-fill, 640px)"
    "grid-gap" -: "30px"
    "grid-auto-rows" -: "min-content"
  ".goalpost" ? do
    (C.h2 <> C.h3) ? do
      C.marginTop (C.px 0)
      C.marginBottom (C.px 5)
    C.header ? do
      C.textAlign C.center
      "background" -: "linear-gradient(to top left, #252525, #282828)"
    (C.header <> C.p) ? do
      C.sym C.padding (C.px 20)
    C.border (C.px 1) C.solid (C.rgb 0x33 0x33 0x33)

goalHtml :: Goal -> H.Html
goalHtml goal = do
  (H.div ! A.class_ "goalpost") do
    H.header do
      H.h2 (H.text goal.title)
      H.h3 (H.text goal.subtitle)
    goal.description