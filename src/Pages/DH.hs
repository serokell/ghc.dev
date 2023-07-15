{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.DH where
import Data.ByteString.Lazy qualified as BS
import Pages.Graph

roadmap :: Roadmap
roadmap = runRoadmapBuilder mdo
  setGoalLevels [
      [openEvaluator, closedEvaluatior],
      [typelevelCleanup],
      [typeSyntaxInTerms],
      [standaloneKindSignatures, invisibleBindersInTypes]
    ]

  dh <- mkGoal
    Goalpost {
      title = "Dependent Haskell",
      description = "Dependent types in GHC",
      completed = False
    } [dependentQuantifiaction, functionPromotion, typelevelCleanup]

  dependentQuantifiaction <- mkGoal
    Goalpost {
      title = "Dependent retained quantifiers",
      description = "Types that refer to terms",
      completed = False
    } [dependentProducts, dependentSums]

  dependentProducts <- mkGoal
    Goalpost {
      title = "Dependent products",
      description = "Π-types",
      completed = False
    } [dependentCore, visibleForall]

  dependentSums <- mkGoal
    Goalpost {
      title = "Dependent sums",
      description = "Σ-types",
      completed = False
    } [dependentCore]

  dependentCore <- mkGoal
    Goalpost {
      title = "Dependent Core language",
      description = "Formalism and implementation\nof the dependently typed Core language",
      completed = False
    } []

  functionPromotion <- mkGoal
    Goalpost {
      title = "Function promotion",
      description = "Promote term-level functions",
      completed = False
    } [closedEvaluatior, openEvaluator]

  closedEvaluatior <- mkGoal
    Goalpost {
      title = "Closed evaluator",
      description = "High-performance closed term evaluator\nusing bytecode or native code",
      completed = False
    } []

  openEvaluator <- mkGoal
    Goalpost {
      title = "Open evaluator",
      description = "Open term evaluator based\non normalization by evaluation",
      completed = False
    } []

  typelevelCleanup <- mkGoal
    Goalpost {
      title = "Type-level cleanup",
      description = "Clean up existing type-level programming features",
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
      description = "Specify the kind and enable polymorphic recursion\nusing a `type T :: k` signature",
      completed = True
    } []

  invisibleBindersInTypes <- mkGoal
    Goalpost {
      title = "Invisible binders in type declarations",
      description = "@k-binders in data, newytpe, class, type synonym,\nand type/data family declarations",
      completed = True
    } []

  invisibleBindersInTerms <- mkGoal
    Goalpost {
      title = "Invisible binders in term-level expressions",
      description = "@a-binders in function equation left-hand sides and lambdas",
      completed = False
    } []

  typeSyntaxInTerms <- mkGoal
    Goalpost {
      title = "Type syntax in terms",
      description = "Construct types in term-level contexts",
      completed = False
    } [visibleForall]

  visibleForall <- mkGoal
    Goalpost {
      title = "Visible forall in types of terms",
      description = "The `forall x -> t` quantifier in types of terms",
      completed = False
    } [typeSyntaxInTerms]

  pure dh

testGraphviz :: IO ()
testGraphviz = BS.writeFile "out/res.svg" =<< extractToSvg roadmap
