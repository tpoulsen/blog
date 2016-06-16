{-# LANGUAGE OverloadedStrings #-}
import Clay

main :: IO()
main = putCss $ do
  bodyStyle
  headerStyle
  footerStyle
  logoStyle
  headerTextStyles
  infoStyle

bodyStyle :: Css
bodyStyle =
  body ? do
    color    black
    fontSize (px 16)
    margin   (px 0) auto (px 0) auto
    width    (px 900)

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


headerTextStyles :: Css
headerTextStyles = do
  h1 ? fontSize (px 24)
  h2 ? fontSize (px 20)

infoStyle :: Css
infoStyle =
  Clay.div # ".info" ? do
    color    "#555"
    fontSize (px 14)
    fontStyle italic
