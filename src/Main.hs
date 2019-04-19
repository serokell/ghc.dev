module Main where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Clay as C
import Clay ((?), (-:))
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as ByteString
import Data.String (IsString(fromString))
import Data.Foldable (traverse_)

main :: IO ()
main = putByteString (renderMarkup mainPage)

putByteString :: ByteString.ByteString -> IO ()
putByteString s =
  Text.writeFile "index.html" (Text.decodeUtf8 (ByteString.toStrict s))

mainPage :: H.Html
mainPage = do
  (H.docTypeHtml ! A.lang "en") do
    H.head do
      H.meta ! A.charset "utf-8"
      H.meta
        ! A.name "viewport"
        ! A.content "width=device-width, initial-scale=1.0"
      H.title "GHC Development"
      styleEmbedCss mainStyle
    H.body do
      (H.div ! A.class_ "topics") do
        traverse_ topicHtml topics

mainStyle :: C.Css
mainStyle = do
  C.body ? C.color "#333333"
  C.a ? C.color C.inherit
  C.code ? C.fontFamily [] [C.monospace]
  ".nowrap" ? C.whiteSpace C.nowrap
  ".snippet" ? do
    C.display C.block
    C.background (C.rgba 0 0 0 0.4)
    C.sym C.padding (C.em 1)
    C.marginTop (C.em 1)
    C.marginBottom (C.em 1)
  ".prompt" ? do
    let m = 2
    C.marginLeft (C.em m)
    C.textIndent (C.indent (C.em (negate m)))
  ".prompt::before" ? C.content (C.stringContent "$ ")
  ".topics" ? do
    C.display C.grid
    C.justifyContent C.center
    "grid-template-columns" -: "repeat(auto-fill, 320px)"
    "grid-gap" -: "10px"
  ".topic" ? C.sym C.padding (C.px 20)
  traverse_ topicCss topics

topicHtml :: Topic -> H.Html
topicHtml Topic{topicName, topicTitle, topicContent} = do
  let topicClasses = fromString ("topic topic-" <> topicName)
  (H.div ! A.class_ topicClasses) do
    H.h2 topicTitle
    topicContent

topicCss :: Topic -> C.Css
topicCss Topic{topicName, topicStyle} =
  fromString (".topic-" <> topicName) ? topicStyle

styleEmbedCss :: C.Css -> H.Html
styleEmbedCss = H.style . H.preEscapedToHtml . C.renderWith C.compact []

ndash :: H.Html
ndash = "–"

commaList :: [H.Html] -> H.Html -> H.Html
commaList [] z = z
commaList (x:xs) z = x <> ", " <> commaList xs z

data Ref =
  RefGitMaster |
  RefUsersGuide |
  RefLibrariesHaddock |
  RefGHC_API_Haddock |
  RefGitLab |
  RefReportBug |
  RefGhcProposals |
  RefGhcDevsMailingList

ref :: IsString s => Ref -> s
ref = \case
  RefGitMaster -> "https://gitlab.haskell.org/ghc/ghc/commits/master"
  RefUsersGuide -> "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/"
  RefLibrariesHaddock -> "https://ghc.gitlab.haskell.org/ghc/doc/libraries/"
  RefGHC_API_Haddock -> "https://ghc.gitlab.haskell.org/ghc/doc/libraries/ghc-8.9/"
  RefGitLab -> "https://gitlab.haskell.org/ghc/ghc"
  RefReportBug -> "https://gitlab.haskell.org/ghc/ghc/issues"
  RefGhcProposals -> "https://github.com/ghc-proposals/ghc-proposals"
  RefGhcDevsMailingList -> "https://mail.haskell.org/pipermail/ghc-devs/"

data Topic =
  Topic
    { topicName :: String,
      topicTitle :: H.Html,
      topicContent :: H.Html,
      topicStyle :: C.Css }

topics :: [Topic]
topics =
  [ topicGreetings,
    topicDocs,
    topicCode,
    topicBuild,
    topicCommunication ]

topicGreetings :: Topic
topicGreetings =
  Topic
    { topicName = "greetings",
      topicTitle = "Greetings!",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "Follow GHC development, use the development compiler snapshot for\
        \ your daring experiments, or become a contributor."
    topicStyle = do
      "grid-column" -: "1 / -1"
      "background" -: "linear-gradient(to bottom left, #90A4AE, #ECEFF1)"

topicDocs :: Topic
topicDocs =
  Topic
    { topicName = "docs",
      topicTitle = "Documentation",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "Documentation for "
        H.a "master" ! A.href (ref RefGitMaster)
        " is built with GitLab CI:"
      H.p do
        H.a "User's Guide" ! A.href (ref RefUsersGuide)
        " " <> ndash <> " "
        commaList ["command line options", "language extensions"] "and so on"
        "."
      H.p do
        H.a "Libraries" ! A.href (ref RefLibrariesHaddock)
        " " <> ndash <> " Haddock for "
        commaList
          (map H.code ["base", "containers", "transformers"])
          "and other boot libraries"
        "."
      H.p do
        H.a "GHC API" ! A.href (ref RefGHC_API_Haddock)
        " " <> ndash <> " use GHC as a library."
    topicStyle = do
      "background" -: "linear-gradient(to left, #009688, #00695C)"
      C.color "#EDEDED"

prompt :: [H.Html] -> H.Html
prompt parts = (H.div ! A.class_ "prompt") (go parts)
  where
    go [] = mempty
    go (x:xs) = x <> " " <> go xs

nowrap :: (H.Html -> H.Html) -> H.Html -> H.Html
nowrap = (! A.class_ "nowrap")

snippet :: H.Html -> H.Html
snippet = H.code ! A.class_ "snippet"

topicCode :: Topic
topicCode =
  Topic
    { topicName = "code",
      topicTitle = "Getting the Code",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "GHC has its own "
        H.a "GitLab instance" ! A.href (ref RefGitLab)
        ". You can sign in with your GitHub account."
      snippet do
        prompt
          [ "git", "clone", nowrap H.span "--recursive",
            "git@gitlab.haskell.org:ghc/" <> H.wbr <> "ghc.git" ]
      H.p do
        H.code "--recursive" <> " is needed because GHC uses git submodules."
    topicStyle = do
      "background" -: "linear-gradient(to bottom, #311B92, #9C27B0)"
      C.color "#EDEDED"

topicBuild :: Topic
topicBuild =
  Topic
    { topicName = "build",
      topicTitle = "Building",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p "For the first time:"
      snippet do
        prompt ["./boot && ./configure"]
        prompt ["cabal", "v2-update"]
        prompt
          [ "hadrian/build.sh", "-j",
            nowrap H.span "--flavour=Quick" ]
      H.p ("Quick " <> H.code "stage2" <> " rebuild:")
      snippet do
        prompt
          [ "hadrian/build.sh", "-j",
            nowrap H.span "--flavour=Quick",
            nowrap H.span "--freeze1" ]
    topicStyle = do
      "background" -: "linear-gradient(to bottom, #E65100, #F9A825)"
      C.color "#EDEDED"

topicCommunication :: Topic
topicCommunication =
  Topic
    { topicName = "communication",
      topicTitle = "Communication",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "To report a bug, use the "
        H.a "GitLab issue tracker" ! A.href (ref RefReportBug)
        "."
      H.p do
        "To propose a new language feature, the "
        H.a (nowrap H.code "ghc-proposals") ! A.href (ref RefGhcProposals)
        " platform."
      H.p do
        "For a technical discussion or a question, the "
        H.a (nowrap H.code "ghc-devs") ! A.href (ref RefGhcDevsMailingList)
        " mailing list or the "
        H.code "#ghc" <> " channel on Freenode (IRC)."
    topicStyle = do
      "background" -: "linear-gradient(to top left, #4FC3F7, #F9FBE7)"
