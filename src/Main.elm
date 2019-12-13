module Main exposing (..)

import Types exposing (..)
import Layout exposing (..)
import Message exposing (..)
import Json exposing (..)
import Browser
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, map2, int, string, decodeString, errorToString, list) -- maybe not in type module

import List

-- MAIN
main =
    Browser.element { init = init, update = update, subscriptions =  subscriptions, view = view }


init : () -> (Model, Cmd Msg)
init _ = ({routines = Nothing , status = Loading }, fetchRoutines)


-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let ((newStatus, cmd), routines) =
         case msg of
             GotAll result ->
                 let (s,c) = fromResult result routinesInfoDecoder SuccessList
                 in
                     case s of
                         SuccessList rs -> ((s,c), Just rs)
                         _ -> ((s,c), model.routines)

             GotSingle result ->
                 (fromResult result routineDecoder View, model.routines)
             FetchAll ->
                 ((Loading, fetchRoutines), model.routines)
             FetchOne id ->
                 (fetchRoutine id, model.routines)
    in
      ({routines = routines, status = newStatus}, cmd)

fromResult : (Result Http.Error String) -> Decoder a -> (a -> Status) -> (Status, Cmd Msg)
fromResult result decoder ret  =
    case result of
        Ok allText ->
            case decodeString decoder allText of
                Ok routines ->
                    (ret routines, Cmd.none)
                Err r ->
                    (Failure (ParsingError (getErrorString r)), Cmd.none)
        Err _ ->
            (Failure LoadingError, Cmd.none)




fetchRoutines : Cmd Msg
fetchRoutines  = Http.get {url = "http://localhost:3000/routines"
                          , expect = Http.expectString GotAll
                          }


fetchRoutine : Int -> (Status, Cmd Msg)
fetchRoutine id = (Loading
                  , Http.get {url = "http://localhost:3000/routines/" ++ (String.fromInt id)
                             , expect = Http.expectString GotSingle
                             }
                  )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view = viewMain
