module Main where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Clay as C
import qualified Clay.Media as C.Media
import Clay ((?), (-:))
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as ByteString
import Data.String (IsString(fromString))
import Data.Foldable (traverse_)

import Data.Time.Clock
import Data.Time.Calendar

import System.Environment
import System.Directory
import System.FilePath
import System.Exit

main :: IO ()
main = do
  outputDir <- getArgs >>= \case
    [dir] -> return dir
    _ -> die "Usage: ghc-dev-webgen DIR"
  createDirectoryIfMissing True outputDir
  time <- getCurrentTime
  putByteString (outputDir </> "index.html") (renderMarkup (mainPage time))

putByteString :: FilePath -> ByteString.ByteString -> IO ()
putByteString path s =
  Text.writeFile path (Text.decodeUtf8 (ByteString.toStrict s))

mainPage :: UTCTime -> H.Html
mainPage time = do
  (H.docTypeHtml ! A.lang "en") do
    H.head do
      H.meta ! A.charset "utf-8"
      H.meta
        ! A.name "viewport"
        ! A.content "width=device-width, initial-scale=1.0"
      H.title "GHC Development"
      styleEmbedCss mainStyle
    H.body do
      (H.div ! A.class_ "header") do
        headerHtml
      (H.div ! A.class_ "topics") do
        traverse_ topicHtml topics
      (H.div ! A.class_ "footer") do
        footerHtml time

headerHtml :: H.Html
headerHtml = do
  H.h1 "The Glasgow Haskell Compiler"
  H.h2 "a contributor's cheatsheet"

footerHtml :: UTCTime -> H.Html
footerHtml time = do
  H.p do
    H.a "Serokell" ! A.href "https://serokell.io/"
    ", " <> fromString (showGregorian (utctDay time))
    ". "
    H.a "Source on GitHub"  ! A.href "https://github.com/serokell/ghc.dev/"

mainStyle :: C.Css
mainStyle = do
  C.body <> C.html ? do
    C.sym C.margin C.nil
    C.sym C.padding C.nil
    "font-family" -: "system-ui,-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif"
  C.body ? do
    C.backgroundColor "#222222"
    C.color "#CDCDCF"
    C.minHeight (C.vh 100)
    C.display C.flex
    C.flexDirection C.column
  C.a ? C.color C.inherit
  C.code ? C.fontFamily [] [C.monospace]
  ".nowrap" ? C.whiteSpace C.nowrap
  snippetCss
  ".header" ? do
    C.color "#EDEDED"
    C.sym C.padding (C.px 20)
    "background" -: "linear-gradient(to bottom right, #3A2A85 30%, transparent)"
    C.textAlign C.center
  ".topics" ? do
    "flex" -: "1"
    C.sym C.padding (C.px 20)
    C.display C.grid
    C.justifyContent C.center
    "grid-template-columns" -: "repeat(auto-fill, 320px)"
    "grid-gap" -: "20px"
    "grid-auto-rows" -: "min-content"
  C.query C.all [C.Media.minWidth (C.px 1333)] $ do
    ".topics" ? do
      "grid-template-columns" -: "repeat(4, 320px)"
  ".topic" ? do
    C.sym C.padding (C.px 20)
    "background" -: "linear-gradient(to top left, #222222, #333333)"
    "border" -: "1px solid #333333"
  traverse_ topicCss topics
  ".footer" ? do
    C.color "#888888"
    C.textAlign C.center

snippetCss :: C.Css
snippetCss = do
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
  RefGHC_Wiki |
  RefGitLab |
  RefGitLabMergeRequest |
  RefReportBug |
  RefGhcProposals |
  RefGhcDevsMailingList |
  RefNix |
  RefGHC_Nix

ref :: IsString s => Ref -> s
ref = \case
  RefGitMaster -> "https://gitlab.haskell.org/ghc/ghc/commits/master"
  RefUsersGuide -> "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/"
  RefLibrariesHaddock -> "https://ghc.gitlab.haskell.org/ghc/doc/libraries/"
  RefGHC_Wiki -> "https://gitlab.haskell.org/ghc/ghc/wikis/home"
  RefGitLab -> "https://gitlab.haskell.org/ghc/ghc"
  RefGitLabMergeRequest -> "https://gitlab.haskell.org/ghc/ghc/wikis/Contributing-a-Patch#opening-a-merge-request"
  RefReportBug -> "https://gitlab.haskell.org/ghc/ghc/issues"
  RefGhcProposals -> "https://github.com/ghc-proposals/ghc-proposals"
  RefGhcDevsMailingList -> "https://mail.haskell.org/pipermail/ghc-devs/"
  RefNix -> "https://nixos.org/nix/"
  RefGHC_Nix -> "https://github.com/alpmestan/ghc.nix"

data Topic =
  Topic
    { topicName :: String,
      topicTitle :: H.Html,
      topicContent :: H.Html,
      topicStyle :: C.Css }

topics :: [Topic]
topics =
  [ topicDocs,
    topicCode,
    topicSystem,
    topicBuild,
    topicRunning,
    topicTesting,
    topicDebugging,
    topicBuildingDocs,
    topicCommunication ]

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
        "The "
        H.a "Wiki" ! A.href (ref RefGHC_Wiki)
        " is a comprehensive resource about GHC development."
        " Use it when this cheatsheet is insufficient."
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
    topicStyle = do pure ()

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
          [ "git", "clone", nowrap H.span "--recurse-submodules",
            "https://gitlab.haskell.org/" <> H.wbr <> "ghc/ghc.git" ]
      H.p do
        nowrap H.code "git pull"
        " also requires "
        nowrap H.code "--recurse-submodules"
        " because GHC uses git submodules."
    topicStyle = do pure()

topicSystem :: Topic
topicSystem =
  Topic
    { topicName = "system",
      topicTitle = "Preparing the System",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        H.a "Nix" ! A.href (ref RefNix)
        " users are fortunate to have "
        H.a (H.code "ghc.nix") ! A.href (ref RefGHC_Nix)
        ":"
      snippet do
        prompt [ "git", "clone", "https://github.com/alpmestan/" <> H.wbr <> "ghc.nix" ]
        prompt [ "nix-shell ghc.nix" ]
      H.p do
        "This will install "
        H.code "alex" <> ", "
        H.code "happy" <> ", "
        H.code "texlive" <> ", "
        "and other build dependencies."
    topicStyle = do pure()

topicBuild :: Topic
topicBuild =
  Topic
    { topicName = "build",
      topicTitle = "Building the Compiler",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p "For the first time:"
      snippet do
        prompt ["./boot && ./configure"]
        prompt ["cabal", "v2-update"]
        prompt
          [ "hadrian/build", "-j",
            nowrap H.span "--flavour=Quick" ]
      H.p ("Quick " <> H.code "stage2" <> " rebuild:")
      snippet do
        prompt
          [ "hadrian/build", "-j",
            nowrap H.span "--flavour=Quick",
            nowrap H.span "--freeze1" ]
    topicStyle = do pure()

topicRunning :: Topic
topicRunning =
  Topic
    { topicName = "running",
      topicTitle = "Running",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "The build artifacts are stored in the "
        H.code "_build"
        " directory."
      H.p "Run the freshly built GHCi:"
      snippet do
        prompt [ nowrap H.span "_build/stage1/bin/ghc",
                 nowrap H.span "--interactive"]
    topicStyle = do pure()

topicBuildingDocs :: Topic
topicBuildingDocs =
  Topic
    { topicName = "build-docs",
      topicTitle = "Building the User's Guide",
      topicContent,
      topicStyle
    }
  where
    topicContent = do
      H.p "To build the User's Guide and Haddock documentation for boot libraries:"
      snippet do
        prompt
          [ "hadrian/build", "-j",
            nowrap H.span "--flavour=Quick",
            nowrap H.span "--freeze1",
            nowrap H.span "docs --docs=no-sphinx-pdfs" ]
      H.p "The generated HTML documentation is saved at:"
      snippet "_build/docs/html/index.html"
    topicStyle = do pure ()

topicTesting :: Topic
topicTesting =
  Topic
    { topicName = "testing",
      topicTitle = "Testing",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p "Run a particular set of tests:"
      snippet do
        prompt
          [ "hadrian/build", "-j",
            nowrap H.span "--flavour=Quick",
            nowrap H.span "--freeze1",
            "test",
            nowrap H.span "--only=\"T1 T2 T3\"" ]
      H.p do
        "Use "
        H.code "-a"
        " to accept the output of failing tests."
      H.p do
        "Omit "
        H.code "--only"
        " to run the entire testsuite."
    topicStyle = do pure()

topicDebugging :: Topic
topicDebugging =
  Topic
    { topicName = "debugging",
      topicTitle = "Debugging",
      topicContent,
      topicStyle }
  where
    topicContent = do
      H.p do
        "Pass the "
        nowrap H.code "-ddump-tc-trace"
        " flag to dump the type checker debug output; "
        nowrap H.code "-ddump-rn-trace"
        " for the renamer."
      H.p "Build GHC with assertions enabled:"
      snippet do
        prompt
          [ "hadrian/build", "-j",
            nowrap H.span "--flavour=Devel2" ]
    topicStyle = do pure()

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
        "To contribute a change, open a "
        H.a "GitLab merge request" ! A.href (ref RefGitLabMergeRequest)
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
    topicStyle = do pure()
