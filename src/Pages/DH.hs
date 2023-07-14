{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.DH where
import Control.Lens
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Text (Text)
import GHC.Generics ( Generic )
import Control.Monad.State
import NeatInterpolation
import Data.Text.IO qualified as TIO

newtype GoalId =
  MkGoalId {
    rawId :: Int
  } deriving newtype (Show, Eq, Ord)

data Goal =
  Goalpost {
    title :: Text,
    description :: Text,
    completed :: Bool
  } deriving (Show, Generic)

data Roadmap =
  MkRoadmap {
    root :: GoalId,
    dependencies :: Set (GoalId, GoalId),
    goals :: Map GoalId Goal
  } deriving (Show, Generic)

data BuilderState =
  MkBuilderState {
    dependencies :: Set (GoalId, GoalId),
    goals :: Map GoalId Goal,
    nextId :: Int
  } deriving (Show, Generic)

type RoadmapBuilder = State BuilderState

runRoadmapBuilder :: RoadmapBuilder GoalId -> Roadmap
runRoadmapBuilder action = MkRoadmap {..} where
  (root, MkBuilderState {dependencies, goals}) = runState action (MkBuilderState mempty mempty 0)

mkGoal :: Goal -> [GoalId] -> RoadmapBuilder GoalId
mkGoal goal subgoals = do
  goalId <- #nextId <<+= 1 <&> MkGoalId
  #goals . at goalId ?= goal
  #dependencies <>= Set.fromList [(goalId, subgoalId) | subgoalId <- subgoals]
  pure goalId

roadmap :: Roadmap
roadmap = runRoadmapBuilder mdo
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
      description = "The `forall x -&gt; t` quantifier in types of terms",
      completed = False
    } [typeSyntaxInTerms]

  pure dh

extractToGraphviz :: Roadmap -> Text
extractToGraphviz MkRoadmap{dependencies, goals} =
  [trimming|
    digraph G {
      ranksep=0.5;
      node [shape = box];
      fontname="Comic Sans MS"; // don't work?
      // goals
      $renderedGoals;
      // graph
      $renderedGraph;
    }
  |] where
  renderedGoals = T.intercalate ";\n" (map extractGoalInfo (Map.toList goals))
  renderedGraph = T.intercalate ";\n" (map extractDependencyGraph (Set.toList dependencies))

  extractGoalInfo (showT -> nodeId, Goalpost{title, description, completed}) =
    [trimming|
      "$nodeId" [
        label=<
          <FONT POINT-SIZE="20"> <B> $title </B> </FONT> <BR ALIGN="CENTER"/>
          $description
        >,
        fillcolor="#222222:#333333",
        fontcolor="#CDCDCF",
        style="filled",
        gradientangle=315,
        color="$color"
        ]
      |] where
    color = if completed then "violet" else "black"


  extractDependencyGraph (showT -> nodeParent, showT -> nodeChild) =
    [trimming|
      "$nodeParent" -> "$nodeChild" [color="#cdcdcf"]
      |]

  showT = T.pack . show

testGraphviz :: IO ()
testGraphviz = TIO.writeFile "out/res.dot" (extractToGraphviz roadmap)
