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
import Data.Text.Lazy qualified as T.L
import Data.Char ( isSpace )
import Data.List qualified
import Data.ByteString.Lazy qualified as BS.L
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as S8
import Data.Binary.Builder ( toLazyByteString )
import GHC.Generics ( Generic )
import System.Process.Typed
import NeatInterpolation
import Xmlbf.Xeno
import Xmlbf
import Xeno.SAX ( skipDoctype )
import Data.Int
import qualified GI.Cairo.Render as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo

import Pages.Common ( cairoFontFamily, cssFontFamily )

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

  extractGoalInfo (showT -> nodeId, Goalpost{completed}) =
    [trimming|
      "$nodeId" [
        label="",
        size = "0.75,1.25",
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
  -- TODO: preprocess svg to remove <title> nodes
  case exitCode of
    ExitSuccess ->
      case fromRawXml $ skipDoctype $ skipXml $ BS.L.toStrict rawSvg of
        Left err -> do
          BS.L.writeFile "error.svg" rawSvg
          fail err
        Right nodes ->
          pure (toLazyByteString $ encode (postProcessSvg =<< nodes))
    ExitFailure _ -> do
      BS.L.writeFile "error.dot" input
      fail "Cannot create SVG from dot. Dumped dot-file to the ./error.dot"
  where
    dot =  setStdin (byteStringInput input) $ proc "dot" ["-Tsvg"]
    input = BS.L.fromStrict $ T.encodeUtf8 $ extractToGraphviz roadmap

postProcessSvg :: Node -> [Node]
postProcessSvg = dfpos \_ -> \case

  Element "g" _ childrens -- Remove mock nodes
    | Just (Element _ _ [Text t]) <- Data.List.find isTitle childrens
    , "mock" `T.L.isPrefixOf` t
    -> mempty

  Element "svg" attrs childrens -- remove "width" and "heigh" from svg node
    -> Xmlbf.element "svg" (attrs & at "width" .~ Nothing & at "height" .~ Nothing) childrens

  Element "text" attrs childrens -- setup font
    | Just "Times,serif" <- attrs ^. at "font-family"
    -> Xmlbf.element "text" (attrs & at "font-family" .~ Just cssFontFamily) childrens

  x | isTitle x -> mempty -- remove title from nodes

  x -> pure x -- all other stuff should be unchanged

  where
    isTitle (Element "title" _ _) = True
    isTitle _ = False


-- | Skip initial ?xml declaration
--
-- This is a lot like `skipDoctype`, but for ?xml
-- See https://github.com/ocramz/xeno/pull/66
skipXml :: BS.ByteString -> BS.ByteString
skipXml arg =
    if "<?xml" `S8.isPrefixOf` bs
      then let (_, rest)=">" `S8.breakSubstring` bs
           in skipBlanks $ S8.drop 1 rest
      else bs
  where
    bs = skipBlanks arg
    skipBlanks = S8.dropWhile isSpace


todo_cairo_example :: IO ()
todo_cairo_example = do
  fontDesc <- createFontDescription cairoFontFamily 12000
  Cairo.withSVGSurface "hello.svg" 20000 20000 \svgSurface -> do
    Cairo.renderWith svgSurface $ do
      (_, _, _, renderText) <- createTextLayout fontDesc "Hello, World"
      renderText (0, 0)
    Cairo.surfaceFinish svgSurface

createFontDescription :: Text -> Int -> IO Pango.FontDescription
createFontDescription fontFamily fontSize = do
  fontDescription <- Pango.fontDescriptionNew
  Pango.fontDescriptionSetFamily fontDescription fontFamily
  Pango.fontDescriptionSetSize fontDescription (fromIntegral fontSize)
  Pango.fontDescriptionSetWeight fontDescription Pango.WeightNormal
  return fontDescription

createTextLayout :: Pango.FontDescription -> Text -> Cairo.Render (Int, Int, Int, (Double, Double) -> Cairo.Render ())
createTextLayout fontDescription str = do
  pangoLayout <- Cairo.getContext >>= PangoCairo.createLayout
  Pango.layoutSetText pangoLayout str (-1)
  Pango.layoutSetFontDescription pangoLayout (Just fontDescription)
  (_logicalExtents, inkExtents) <- Pango.layoutGetExtents pangoLayout
  inkW <- Pango.getRectangleWidth inkExtents
  inkH <- Pango.getRectangleHeight inkExtents
  pangoIter <- Pango.layoutGetIter pangoLayout
  pangoBaseline <- Pango.layoutIterGetBaseline pangoIter
  let renderText (x, y) = do
        cairoContext <- Cairo.getContext
        Cairo.moveTo x y
        Cairo.setSourceRGB 0 0 1
        PangoCairo.showLayout cairoContext pangoLayout
  return (toPixels pangoBaseline, toPixels inkW, toPixels inkH, renderText)

toPixels :: Int32 -> Int
toPixels a = fromIntegral (a `div` Pango.SCALE)