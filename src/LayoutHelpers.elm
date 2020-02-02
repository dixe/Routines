module LayoutHelpers exposing(..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Message exposing (..)




spacerLine : Element Msg
spacerLine =
    column [width fill]
        [
         el [ width fill
            , Border.width 1
            , Border.color gray
            ]
             none
        , el [ paddingXY 0 10 ] none
        ]

buttonLayout : (List (Attribute Msg))
buttonLayout = [ Background.color buttonColor
               , Font.color black
               , Border.color black
               , paddingXY 16 10
               , Border.rounded 3
               , Font.size 20
               ]



listButtonLayout : List (Attribute Msg)
listButtonLayout = [ Background.color buttonColor
               , Font.color black
               , Border.color black
               , paddingXY 10 5
               , Border.rounded 3
               , Font.size 20
               ]



panelLayout : List (Attribute Msg)
panelLayout = [ centerY
              , centerX
              , padding 10
              , spacing 10
              , height fill
              , width fill
              , Border.rounded 4
              , Background.color panelBackgroundColor
              ]

-- COLORS

buttonColor = darkGray

panelBackgroundColor = gray


white = Element.rgb 1 1 1

black = Element.rgb 0  0 0

gray = Element.rgb 0.9 0.9 0.9

darkGray = Element.rgb 0.8 0.8 0.8

blue = Element.rgb 0 0 0.8

red = Element.rgb 0.8 0 0

darkBlue = Element.rgb 0 0 0.9
