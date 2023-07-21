{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Pages.Graph where
import Control.Lens
import Control.Monad.State
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Set ( Set )
import Data.Map qualified as Map
import Data.Map ( Map )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text ( Text )
import Data.Text.Lazy qualified as T.L
import Data.Char ( isSpace, isDigit )
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
import Data.Traversable
import Debug.Trace
import Text.Read

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

extractToGraphviz :: Roadmap -> GoalTextItems -> Text
extractToGraphviz MkRoadmap{dependencies, goalLevels, goals} goalTextItems =
  [trimming|
    digraph G {
      ranksep=0.5;
      dpi = 96;
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

  extractGoalInfo (nodeId, Goalpost{completed}) =
    [trimming|
      "$nodeIdText" [
        label="",
        width = $wSize,
        height = $hSize,
        fixedsize = true,
        color="$color"
        ]
      |] where
    color = if completed then "#593fcf" else "#333333"
    nodeIdText = showT nodeId
    textItem = goalTextItems ^. at nodeId
    hSize = showT $ maybe (trace "default\n" 0.75) (pixelsToInches . (.sizeH)) textItem
    wSize = showT $ maybe 1.25 (pixelsToInches . (.sizeW)) textItem
    pixelsToInches = (/96)

  extractGoalLevel (goalLevel, map showT -> semicolonSeparated -> oneRankGoals) =
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

extractGraphToSvg :: Roadmap -> GoalTextItems -> Cairo.Render BS.L.ByteString
extractGraphToSvg roadmap goalTextItems = do
  (exitCode, rawSvg) <- liftIO $ readProcessStdout dot
  case exitCode of
    ExitSuccess ->
      case fromRawXml $ skipDoctype $ skipXml $ BS.L.toStrict rawSvg of
        Left err -> do
          liftIO $ BS.L.writeFile "error.svg" rawSvg
          liftIO $ fail err
        Right [_, Element "svg" attrs nodes, _] -> do
          svg <- traverse (postProcessSvg (parseHeight attrs) goalTextItems) nodes
          pure (toLazyByteString $ encode (Xmlbf.element "svg" (updateSvgAttrs attrs) (concat svg)))
        Right _ -> do
          liftIO $ BS.L.writeFile "error.svg" rawSvg
          liftIO $ fail "unexpected nodes"
    ExitFailure _ -> do
      liftIO $ BS.L.writeFile "error.dot" input
      liftIO $ fail "Cannot create SVG from dot. Dumped dot-file to the ./error.dot"
  where
    dot =  setStdin (byteStringInput input) $ proc "dot" ["-Tsvg"]
    input = BS.L.fromStrict $ T.encodeUtf8 $ extractToGraphviz roadmap goalTextItems

    updateSvgAttrs attrs = attrs & at "width" .~ Nothing & at "height" .~ Nothing

    parseHeight attrs = case attrs ^. at "height" of
      Just (T.takeWhile isDigit -> T.unpack -> height) -> read @Double height
      Nothing -> error "no height in svg"

postProcessSvg :: Double -> GoalTextItems -> Node -> Cairo.Render [Node]
postProcessSvg height goalTextItems = dfposM \_ -> \case

  Element "g" _ childrens -- Remove mock nodes
    | Just (Element _ _ [Text t]) <- Data.List.find isTitle childrens
    , "mock" `T.L.isPrefixOf` t
    -> pure mempty

  (Element "g" attrs childrens) -- Render cairo text
    | Just "node" <- attrs ^. at "class"
    , Just (Element _ _ [Text title]) <- Data.List.find isTitle childrens
    , Just (Element _ polygonAttrs _) <- Data.List.find isPolygon childrens
    , Just points <- polygonAttrs ^. at "points"
    , Just goalId <- readMaybe @Int (T.L.unpack title)
    , Just coords <- parseStartPoint points
    , Just textItem <- goalTextItems ^. at (MkGoalId goalId)
    -> do
      liftIO $ print coords
      textItem.createText coords
      pure (Xmlbf.element "g" attrs (filterOut isTitle childrens))

  Element "text" attrs childrens -- setup font
    | Just "Times,serif" <- attrs ^. at "font-family"
    -> pure $ Xmlbf.element "text" (attrs & at "font-family" .~ Just cssFontFamily) childrens

  x -> pure [x] -- all other stuff should be unchanged

  where
    isTitle (Element "title" _ _) = True
    isTitle _ = False

    isPolygon (Element "polygon" _ _) = True
    isPolygon _ = False

    filterOut p = filter (not . p)

    parseStartPoint (
      T.takeWhile (/= ' ') ->
      T.breakOn "," ->
        ( T.unpack -> x,
          T.drop 1 -> T.unpack -> y))
      = (,) <$> (readMaybe @Double x) <*> (fmap (+height) $ readMaybe @Double y)


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

data TextItem =
  MkTextItem {
    sizeH :: Double,
    sizeW :: Double,
    createText :: (Double, Double) -> Cairo.Render ()
  }

type Fonts = (?titleFont :: Pango.FontDescription, ?subtitleFont :: Pango.FontDescription)
type GoalTextItems = Map GoalId TextItem

goalToTextItem :: Fonts => Goal -> Cairo.Render TextItem
goalToTextItem Goalpost{title, subtitle} = do
  (_, wTitle, hTitle, renderTitleText)
    <- createTextLayout ?titleFont title
  (_, wSubtitle, hSubtitle, renderSubtitleText)
    <- createTextLayout ?subtitleFont subtitle
  let
    sizeW = i2d (max wTitle wSubtitle) + 5
    sizeH = i2d hTitle * 1.2 + i2d hSubtitle + 5
    createText = \(x, y) -> do
      let
        xTitle = x + 2.5 + (sizeW - i2d wTitle) / 2
        yTitle = y + 2.5
        xSubtitle = x + 2.5 + (sizeW - i2d wSubtitle) / 2
        ySubitle = y + 2.5 + i2d hTitle * 1.2
      renderTitleText (xTitle, yTitle)
      renderSubtitleText (xSubtitle, ySubitle)
  pure MkTextItem{sizeW, sizeH, createText}
  where
    i2d = fromIntegral @Int @Double

createGoalTextItems :: Fonts => Roadmap -> Cairo.Render GoalTextItems
createGoalTextItems roadmap = for roadmap.goals goalToTextItem

dumpRender :: GoalTextItems -> Cairo.Render ()
dumpRender items = ifor_ items \(MkGoalId goalId) textItem ->
  textItem.createText (0, fromIntegral goalId * 75)

testGoal :: Goal
testGoal =  Goalpost {
      title = "Dependent Haskell",
      subtitle = "dependent types in GHC",
      completed = False
    }

extractToSvg :: Roadmap -> IO BS.L.ByteString
extractToSvg roadmap = do
  titleFont <- createFontDescription cairoFontFamily 16000
  subtitleFont <- createFontDescription cairoFontFamily 12000
  let ?titleFont = titleFont; ?subtitleFont = subtitleFont

  Cairo.withSVGSurface "hello.svg" 20000 20000 \svgSurface -> do
    graphSvg <- Cairo.renderWith svgSurface do
      goalTextItems <- createGoalTextItems roadmap
      extractGraphToSvg roadmap goalTextItems
    Cairo.surfaceFinish svgSurface
    pure graphSvg

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
