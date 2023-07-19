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
      completed = False
    } [dependentQuantification, universalPromotion, typelevelCleanup]

  dependentQuantification <- mkGoal
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
    } [dependentCore, existentialQuantification]

  dependentCore <- mkGoal
    Goalpost {
      title = "Dependent Core language",
      subtitle = "formalism and implementation",
      completed = False
    } []

  universalPromotion <- mkGoal
    Goalpost {
      title = "Universal promotion",
      subtitle = "promote all terms",
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
      completed = False
    } [charPromotion, natPromotion, constrainedTypeFamilies]

  charPromotion <- mkGoal
    Goalpost {
      title = "Promote Char",
      subtitle = "and character literals",
      completed = True
    } []

  natPromotion <- mkGoal
    Goalpost {
      title = "Promote Natural",
      subtitle = "unify it with Nat",
      completed = True
    } []

  unboxedPromotion <- mkGoal
    Goalpost {
      title = "Promote unboxed types",
      subtitle = "RuntimeRep ≠ LiftedRep",
      completed = False
    } [homogeneousEquality]

  homogeneousEquality <- mkGoal
    Goalpost {
      title = "Homogeneous equality",
      subtitle = "instead of heterogeneous",
      completed = False
    } []

  constraintPromotion <- mkGoal
    Goalpost {
      title = "Promote classes",
      subtitle = "constraints and dictionaries",
      completed = False
    } [functionPromotion, constrainedTypeFamilies]

  functionPromotion <- mkGoal
    Goalpost {
      title = "Function promotion",
      subtitle = "promote term-level functions",
      completed = False
    } [closedEvaluatior, openEvaluator, typelevelLambdas]

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

  typelevelLambdas <- mkGoal
    Goalpost {
      title = "Type-level lambdas",
      subtitle = "first-class functions in types",
      completed = False
    } [dependentCore, openEvaluator]

  typelevelCleanup <- mkGoal
    Goalpost {
      title = "Clean up the language",
      subtitle = "preparations for dependent types",
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
      completed = False
    } []

  unsaturatedTypeFamilies <- mkGoal
    Goalpost {
      title = "Unsaturated type families",
      subtitle = "partial application in types",
      completed = False
    } [modifiersSyntax]

  constrainedTypeFamilies <- mkGoal
    Goalpost {
      title = "Constrained type families",
      subtitle = "constraints in kinds",
      completed = False
    } []

  modifiersSyntax <- mkGoal
    Goalpost {
      title = "Modifiers syntax",
      subtitle = "generalized modifiers",
      completed = False
    } []

  standaloneKindSignatures <- mkGoal
    Goalpost {
      title = "Standalone kind signatures",
      subtitle = "replacement for CUSKs",
      completed = True
    } []

  invisibleBinders <- mkGoal
    Goalpost {
      title = "Invisible type variable binders",
      subtitle = "in types and terms",
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
      completed = True
    } [standaloneKindSignatures]

  invisibleBindersInFunctions <- mkGoal
    Goalpost {
      title = "Invisible binders in functions",
      subtitle = "@a-binders in lambdas",
      completed = False
    } []

  invisibleBindersInConstructors <- mkGoal
    Goalpost {
      title = "Invisible binders in constructors",
      subtitle = "@a-binders in constructor patterns",
      completed = True
    } []

  syntacticUnification <- mkGoal
    Goalpost {
      title = "Syntactic unification",
      subtitle = "type syntax in terms",
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
      completed = False
    } []

  noListTuplePuns <- mkGoal
    Goalpost {
      title = "No list or tuple puns",
      subtitle = "remove ambiguous built-in syntax",
      completed = False
    } []

  punWarnings <- mkGoal
    Goalpost {
      title = "Pun warnings",
      subtitle = "warn on puns and pun bindings",
      completed = False
    } [namespaceSpecifiedImports]

  visibleForall <- mkGoal
    Goalpost {
      title = "Visible forall",
      subtitle = "in types of terms",
      completed = False
    } [visCoercions, embedTypes, nestedGadtForalls, syntacticUnification]

  visCoercions <- mkGoal
    Goalpost {
      title = "Visibility coercions",
      subtitle = "in the Core language",
      completed = True
    } []

  nestedGadtForalls <- mkGoal
    Goalpost {
      title = "Nested quantification in GADTs",
      subtitle = "interleave foralls in constructors",
      completed = False
    } []

  embedTypes <- mkGoal
    Goalpost {
      title = "Embed types",
      subtitle = "into expressions",
      completed = False
    } []

  termVariableCapture <- mkGoal
    Goalpost {
      title = "Term variable capture",
      subtitle = "mention term variables in types",
      completed = False
    } []

  noStarIsType <- mkGoal
    Goalpost {
      title = "No star kind syntax",
      subtitle = "resolve conflict with operators",
      completed = True
    } [typeInType]

  forallKeyword <- mkGoal
    Goalpost {
      title = "The forall keyword",
      subtitle = "at the term level",
      completed = False
    } []

  tildeOperator <- mkGoal
    Goalpost {
      title = "The tilde operator",
      subtitle = "for equality constraints",
      completed = True
    } [whitespaceSensitiveOperators]

  whitespaceSensitiveOperators <- mkGoal
    Goalpost {
      title = "Whitespace-sensitive operators",
      subtitle = "prefix vs infix occurrences",
      completed = True
    } []

  noKindVars <- mkGoal
    Goalpost {
      title = "Unify type and kind variables",
      subtitle = "consistent quantification rules",
      completed = True
    } [typeInType]

  noCusks <- mkGoal
    Goalpost {
      title = "Deprecate CUSKs",
      subtitle = "complete user-supplied kinds",
      completed = False
    } [invisibleBindersInTypes]

  typeInType <- mkGoal
    Goalpost {
      title = "Unify types and kinds",
      subtitle = "with the Type :: Type axiom",
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