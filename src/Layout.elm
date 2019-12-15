module Layout exposing (viewMain)

import Types exposing (..)
import Exercise exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Message exposing (..)
import Routine exposing (..)

viewMain : Model -> Html Msg
viewMain model =
    Element.layout
        [Font.size 20
        ]
    <|
        column [width fill, height fill]
            [
             el
             [ Region.heading 1
             , centerX
             , alignTop
             , Font.size 36
             , padding 10
             , alignTop
             , height (fillPortion 1)
             ]
             (text "Welcome to routine browser")
            , spacerLine
            , row [alignLeft, alignTop, height (fillPortion 10)]
                [ el [alignTop, height fill, padding 5] (routinesPanel model)
                , el [alignTop, padding 5] (routinePanel model)
                ]
            ]


routinesPanel : Model -> Element Msg
routinesPanel model =
    Element.column [ centerY
                   , centerX
                   , padding 10
                   , spacing 10
                   , height fill
                   , Border.rounded 4

                   , Background.color panelBackgroundColor
                   ]
        [
         Input.button
             buttonLayout
             { onPress = Just FetchAll
             , label = text "Fetch routines"
             }
        , viewRoutineFilter
        , viewRoutines model.routines
        ]


routinePanel : Model -> Element Msg
routinePanel model =
    case model.status of
        Failure problem ->
            displayError problem
        Loading ->
            none
        View r ->
            viewRoutine r
        Run r ->
            text ("running" ++ getName r.routine ++ " Elapsed: " ++ String.fromInt r.elapsed)
        _ ->
            none




viewRoutine : Routine -> Element Msg
viewRoutine r =
    column [ spacing 10

           , Background.color panelBackgroundColor
           , Border.rounded 4
           ]

    [ el [Font.size 25] (text (getName r))
    , viewRoutineExercises (getExercises r)
    , Input.button
        listButtonLayout
        { onPress = Just (StartRoutine)
        , label = text ("Start")
        }
    ]


displayError : Problem -> Element Msg
displayError problem =
    case problem of
        LoadingError -> text "Error loading data"
        ParsingError data -> text ("Error parsing data : " ++ data)


viewRoutines : RoutineListMaybe -> Element Msg
viewRoutines routines = column [ spacing 10
                               , paddingXY 0 10
                               , alignLeft] (List.map viewListedRoutine  (getRoutineListFiltered routines))



viewRoutineFilter : Element Msg
viewRoutineFilter = Input.text
                    [
                     Font.size 20
                    ]
                    { label = Input.labelHidden ""
                    , onChange = Filter
                    , placeholder = Nothing -- Just (Input.placeholder [] (text "Name Filter"))
                    , text = ""
                    }



viewListedRoutine : RoutineInfo -> Element Msg
viewListedRoutine r = Input.button
                      listButtonLayout
                      { onPress = Just (FetchOne (getIdInfo r))
                      , label = text (getNameInfo r)
                      }


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
               , Font.size 24
               ]



listButtonLayout : (List (Attribute Msg))
listButtonLayout = [ Background.color buttonColor
               , Font.color black
               , Border.color black
               , paddingXY 10 5
               , Border.rounded 3
               , Font.size 20
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

darkBlue =Element.rgb 0 0 0.9
