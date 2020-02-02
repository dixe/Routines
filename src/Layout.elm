module Layout exposing (viewMain)

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
import LayoutHelpers exposing (..)
import Types exposing (..)

viewMain : Model -> Html Msg
viewMain model =
    Element.layout
        [Font.size 20
        ]
    <|
        column [width fill, height fill, spacing 10]
            [
             el
             [ Region.heading 1
             , centerX
             , centerY
             , Font.size 36
             , padding 20
             ]
             (text "Welcome to routine browser")
            , spacerLine
            , row [alignLeft, alignTop, height (fillPortion 16), width fill]
                [ el [alignTop, height fill, padding 5, width (fillPortion 1), clip] (routinesPanel model)
                , el [alignTop, height fill, padding 5, width (fillPortion 1), clip] (routinePanel model)
                , el [alignTop, height fill, padding 5, width (fillPortion 6), clip] (runRoutinePanel model)
                ]
            ]


-- PANELS

routinesPanel : Model -> Element Msg
routinesPanel model =
    Element.column
        panelLayout
        [
         Input.button
             buttonLayout
             { onPress = Just FetchAll
             , label = text "Reset"
             }
        , viewRoutineFilter <| getFilterValue model.routines
        , viewRoutines model.routines
        ]


routinePanel : Model -> Element Msg
routinePanel model =
    case model.status of
        Failure problem -> displayError problem
        View r -> el panelLayout <| viewRoutine r
        Run r -> el panelLayout <| viewRoutine r
        _ -> none


runRoutinePanel : Model -> Element Msg
runRoutinePanel model =
    case model.status of
        Run r -> el panelLayout <| viewRunRoutine <| r
        View r ->   Input.button
                    listButtonLayout
                    { onPress = Just (StartRoutine)
                    , label = text ("Start")
                    }
        _ -> none


-- ROUTINES


viewRunRoutine : Routine -> Element Msg
viewRunRoutine r =
    case r.current of
        Nothing -> text  <| "All done!"
        Just e -> viewRunExercise e r.elapsed


viewRoutine : Routine -> Element Msg
viewRoutine r =
    column [ spacing 10

           , Background.color panelBackgroundColor
           , Border.rounded 4
           ]

    [ el [Font.size 25] (text (getName r))
    , viewRoutineExercises (getExercises r)
    ]



viewRoutines : RoutineListMaybe -> Element Msg
viewRoutines routines = column [ spacing 10
                               , paddingXY 0 10
                               , alignLeft
                               , height fill
                               ] (List.map viewListedRoutine (getRoutineListFiltered routines))



viewRoutineFilter : String -> Element Msg
viewRoutineFilter s = Input.text
                      [
                       Font.size 20
                      ]
                      { label = Input.labelHidden ""
                      , onChange = Filter
                      , placeholder = Nothing -- Just (Input.placeholder [] (text "Name Filter"))
                      , text = s
                      }



viewListedRoutine : RoutineInfo -> Element Msg
viewListedRoutine r = Input.button
                      listButtonLayout
                      { onPress = Just (FetchOne (getIdInfo r))
                      , label = text (getNameInfo r)
                      }


-- HELPERS

displayError : Problem -> Element Msg
displayError problem =
    case problem of
        LoadingError -> text "Error loading data"
        ParsingError data -> text ("Error parsing data : " ++ data)
