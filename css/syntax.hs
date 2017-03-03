{-# LANGUAGE OverloadedStrings#-}

import Data.Monoid
import Clay

main :: IO()
main = putCss $ do
  codeBlockStyle
  inLineCodeBlockStyle
  sourceCodeStyle
  sourceCodeTableStyle
  tableCellStyle

codeBlockStyle :: Css
codeBlockStyle =
  pre ? do
    backgroundColor "#fdf6e3"
    padding         (px 10) (px 10) (px 10) (px 10)
    overflowX       scroll
    code <? do
      fontFamily    ["Fira Code", "Monoid"] [monospace]
      fontSize      (em 1.0)

inLineCodeBlockStyle :: Css
inLineCodeBlockStyle =
  code <? do
    fontFamily    ["Fira Code", "Monoid"] [monospace]
    fontSize      (em 1.0)

sourceCodeStyle :: Css
sourceCodeStyle =
  {--Class signifiers
    .kw Keyword
    .dt DataType
    .dv DecVal (decimal values)
    .bn BaseN
    .fl Float
    .ch Char
    .st String
    .co Comment
    .ot OtherToken
    .at AlertToken
    .fu Function
    .re RegionMarker
    .er ErrorTok
  --}
  ".sourceCode" ? do
    Clay.span # ".kw" <? do
      color      "#268bd2"
      fontWeight bold
    Clay.span # ".dt" <? do
      color      "#268bd2"
    Clay.span # ".dv" <? do
      color      "#d33682"
    Clay.span # ".bn" <? do
      color      "#d33682"
    Clay.span # ".fl" <? do
      color      "#d33682"
    Clay.span # ".ch" <? do
      color      "#dc322f"
    Clay.span # ".st" <? do
      color      "#2aa198"
    Clay.span # ".co" <? do
      color      "#93a1a1"
      fontStyle  italic
    Clay.span # ".ot" <? do
      color      "#a57800"
    Clay.span # ".al" <? do
      color      "#cb4b16"
      fontWeight bold
    Clay.span # ".fu" <? do
      color      "#268bd2"
    Clay.span # ".re" <? do
    Clay.span # ".er" <? do
      color      "#d30102"
      fontWeight bold

sourceCodeTableStyle :: Css
sourceCodeTableStyle =
  (
    (table <> tr <> td) # ".sourceCode" <>
    td # ".lineNumbers"
  ) ? do
  margin        (px 0) (px 0) (px 0) (px 0)
  padding       (px 0) (px 0) (px 0) (px 0)
  border        none (px 0) black
  verticalAlign vAlignBaseline

tableCellStyle :: Css
tableCellStyle =
  tr ? do
    td # ".lineNumbers" <? do
      borderRight  solid (px 1) "#AAAAAA"
      textAlign    (alignSide sideRight)
      color        "#AAAAAA"
      paddingRight (px 5)
      paddingLeft  (px 5)
    td # ".sourceCode" <? do
      paddingLeft  (px 5)


