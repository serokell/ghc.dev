module Pages.Common where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Clay as C
import Clay ((?), (-:))

import Data.Time.Clock
import Data.Time.Calendar
import Data.String
import Data.Text (Text)

fontFamily :: Text
fontFamily = "system-ui,-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif"

commonStyle :: C.Css
commonStyle = do
  C.body <> C.html ? do
    C.sym C.margin C.nil
    C.sym C.padding C.nil
    "font-family" -: fontFamily
  C.body ? do
    C.backgroundColor "#222222"
    C.color "#CDCDCF"
    C.minHeight (C.vh 100)
  C.a ? C.color C.inherit
  C.code ? C.fontFamily [] [C.monospace]
  ".nowrap" ? C.whiteSpace C.nowrap
  headerCss
  footerCss

headerCss :: C.Css
headerCss =
  ".header" ? do
    C.color "#EDEDED"
    C.sym C.padding (C.px 20)
    "background" -: "linear-gradient(to bottom right, #3A2A85 30%, transparent)"
    C.textAlign C.center

footerCss :: C.Css
footerCss =
  ".footer" ? do
    C.color "#888888"
    C.textAlign C.center

headerHtml :: Text -> H.Html
headerHtml subtopic = do
  H.h1 "The Glasgow Haskell Compiler"
  H.h2 (H.text subtopic)

footerHtml :: UTCTime -> H.Html
footerHtml time = do
  H.p do
    H.a "Serokell" ! A.href "https://serokell.io/"
    ", " <> fromString (showGregorian (utctDay time))
    ". "
    H.a "Source on GitHub"  ! A.href "https://github.com/serokell/ghc.dev/"

styleEmbedCss :: C.Css -> H.Html
styleEmbedCss = H.style . H.preEscapedToHtml . C.renderWith C.compact []
