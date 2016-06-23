{-# LANGUAGE OverloadedStrings #-}
import Clay

main :: IO()
main = putCss $ do
  bodyStyle
  headerTextStyles
  linkStyle
  imgStyle
  headerStyle
  footerStyle
  logoStyle
  infoStyle
  articleStyle
  tableOfContentsStyle

--------------------------------------------------------------------------------
------------------------------- Generic elements -------------------------------
--------------------------------------------------------------------------------
bodyStyle :: Css
bodyStyle =
  body ? do
    color      black
    fontSize   (px 16)
    margin     (px 0) auto (px 0) auto
    width      (pct 80)
    fontFamily ["Century Gothic", "Arial", "Helvetica"] [sansSerif]
    fontSize   (em 1)
    lineHeight (em 1.5)

headerTextStyles :: Css
headerTextStyles = do
  h1 ? fontSize (px 24)
  h2 ? fontSize (px 20)

linkStyle :: Css
linkStyle =
  a ? do
    textDecoration none


imgStyle :: Css
imgStyle =
  img ? do
    maxWidth (px 900)

--------------------------------------------------------------------------------
------------------------------- Specific elements ------------------------------
--------------------------------------------------------------------------------
headerStyle :: Css
headerStyle =
  Clay.div # "#header" ? do
    borderBottom solid (px 2) black
    marginBottom (px 30)
    padding      (px 12) (px 0) (px 12) (px 0)
    "#navigation" <? do
      textAlign $ alignSide sideRight
      a <? do
      color          black
      fontSize       (px 18)
      fontWeight     bold
      marginLeft     (px 12)
      textDecoration none
      textTransform  uppercase

footerStyle :: Css
footerStyle =
  Clay.div # "#footer" ? do
    borderTop solid (px 2) black
    color           "#555"
    fontSize        (px 12)
    marginTop       (px 30)
    padding         (px 12) (px 0) (px 12) (px 0)
    textAlign       (alignSide sideRight)

logoStyle :: Css
logoStyle =
  Clay.div # "#logo" |> a ? do
    color          black
    float          floatLeft
    fontSize       (px 18)
    fontWeight     bold
    textDecoration none

infoStyle :: Css
infoStyle =
  Clay.div # ".info" ? do
    color    "#555"
    fontSize (px 14)
    fontStyle italic

articleStyle :: Css
articleStyle =
  article # ".post" ? do
    display            $ other "flex"
    "flex-direction"  -: "row-reverse"
    "justify-content" -: "flex-end"
    "flex-wrap"       -: "wrap"
    section # ".toc" <? do
      "flex" -: "0 3 20%"
    section # ".post-body" <? do
      "flex" -: "3 0 80%"

tableOfContentsStyle :: Css
tableOfContentsStyle =
  Clay.section # ".toc" ? do
    float floatRight
