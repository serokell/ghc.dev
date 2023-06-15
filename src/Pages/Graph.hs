{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Graph where
import Control.Lens
import Control.Monad.State
import Control.Arrow ( (>>>) )
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Set ( Set )
import Data.Map qualified as Map
import Data.Map ( Map )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text ( Text )
import Data.ByteString.Lazy qualified as BS.L
import Data.Binary.Builder ( toLazyByteString )
import GHC.Generics ( Generic )
import System.Process.Typed
import NeatInterpolation
import Xmlbf.Xeno
import Xmlbf

import Pages.Common ( cairoFontFamily )

newtype GoalId =
  MkGoalId {
    rawId :: Int
  } deriving newtype (Show, Eq, Ord)

data Goal =
  Goalpost {
    title :: Text,
    subtitle :: Text,
    completed :: Bool
  } deriving (Show, Generic)

data Roadmap =
  MkRoadmap {
    root :: GoalId,
    goalLevels :: [[GoalId]],
    dependencies :: Set (GoalId, GoalId),
    goals :: Map GoalId Goal
  } deriving (Show, Generic)

data BuilderState =
  MkBuilderState {
    dependencies :: Set (GoalId, GoalId),
    goalLevels :: [[GoalId]],
    goals :: Map GoalId Goal,
    nextId :: Int
  } deriving (Show, Generic)

type RoadmapBuilder = State BuilderState

runRoadmapBuilder :: RoadmapBuilder GoalId -> Roadmap
runRoadmapBuilder action = MkRoadmap {..} where
  (root, MkBuilderState {dependencies, goalLevels, goals})
    = runState action (MkBuilderState mempty mempty mempty 0)

setGoalLevels :: [[GoalId]] -> RoadmapBuilder ()
setGoalLevels = (#goalLevels .=)

mkGoal :: Goal -> [GoalId] -> RoadmapBuilder GoalId
mkGoal goal subgoals = do
  goalId <- #nextId <<+= 1 <&> MkGoalId
  #goals . at goalId ?= goal
  #dependencies <>= Set.fromList [(goalId, subgoalId) | subgoalId <- subgoals]
  pure goalId

extractToGraphviz :: Roadmap -> Text
extractToGraphviz MkRoadmap{dependencies, goalLevels, goals} =
  [trimming|
    digraph G {
      ranksep=0.5;
      node
        [
          shape = box,
          fillcolor="#222222:#333333",
          fontcolor="#CDCDCF",
          style="radial",
          gradientangle=135
        ];
      bgcolor="#222222";
      // goals
      $renderedGoals
      //levels
      $renderLevels
      // order levels
      $orderGoals
      // graph
      $renderedGraph
    }
  |] where
  semicolonSeparated = T.intercalate ";\n"

  renderedGoals = semicolonSeparated (map extractGoalInfo (Map.toList goals))
  renderedGraph = semicolonSeparated (map extractDependencyGraph (Set.toList dependencies))
  renderLevels  = semicolonSeparated (map extractGoalLevel (zip goalLevelPoints goalLevels))
  orderGoals = renderLevelPoints goalLevelPoints
  goalLevelPoints = map (\x -> "mock" <> showT x) [1 .. length goalLevels]

  extractGoalInfo (showT -> nodeId, Goalpost{title, subtitle, completed}) =
    [trimming|
      "$nodeId" [
        label=<
          <table border="0">
          <tr><td><font face="$cairoFontFamily" point-size="14">$title</font></td></tr>
          <tr><td><font face="$cairoFontFamily" point-size="12">$subtitle</font></td></tr>
          </table>
        >,
        color="$color"
        ]
      |] where
    color = if completed then "#593fcf" else "#333333"

  extractGoalLevel (goalLevel, map showT >>> semicolonSeparated -> oneRankGoals) =
    [trimming|{ rank="same"; $goalLevel; $oneRankGoals }|]

  extractDependencyGraph (showT -> nodeParent, showT -> nodeChild) =
    [trimming|
      "$nodeParent" -> "$nodeChild" [color="#cdcdcf"]
      |]

  renderLevelPoints levelPoints =
    [trimming|
      {
        edge [style=invis]
        $levelPointOrder
        $levelDescription
      }
    |]
    where
      levelDescription = semicolonSeparated (map stylePoint levelPoints)
      levelPointOrder = T.intercalate "->" levelPoints
      stylePoint point = [trimming|$point [shape=none, width=0, height=0, label=""]|]

  showT :: Show a => a -> Text
  showT = T.pack . show

extractToSvg :: Roadmap -> IO BS.L.ByteString
extractToSvg roadmap = do
  (exitCode, rawSvg) <- readProcessStdout dot
  case exitCode of
    ExitSuccess ->
      case fromRawXml $ BS.L.toStrict rawSvg of
        Left err -> do
          BS.L.writeFile "error.svg" rawSvg
          fail err
        Right [_, Element "svg" attrs ns, _] ->
          pure (toLazyByteString $ encode (Xmlbf.element "svg" (removeSizes attrs) ns))
        Right ns -> do
          BS.L.writeFile "error.svg" rawSvg
          fail ("unexpected count of nodes: " <> show ns)
    ExitFailure _ -> do
      BS.L.writeFile "error.dot" input
      fail "Cannot create SVG from dot. Dumped dot-file to the ./error.dot"
  where
    dot =  setStdin (byteStringInput input) $ proc "dot" ["-Tsvg:cairo"]
    input = BS.L.fromStrict $ T.encodeUtf8 $ extractToGraphviz roadmap

    removeSizes attrs = attrs & at "width" .~ Nothing & at "height" .~ Nothing
