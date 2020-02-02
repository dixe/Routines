module Json exposing (getErrorString, routinesInfoDecoder, routineDecoder, exerciseDecoder)

import Exercise exposing (..)
import Routine exposing (..)
import Json.Decode as Decode exposing (Decoder, Error(..), field, map, map2, map3, map4, int, string, decodeString, errorToString, list, oneOf, decodeValue, succeed, andThen, fail) -- maybe not in type module
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import SmartJson exposing (atdDecoder)



-- EXERCISE DECODERS


restDecoder : Decoder Exercise
restDecoder = Decode.succeed createRest
            |> required "contents" int



repsAndSetsDecoder : Decoder Exercise
repsAndSetsDecoder = Decode.succeed createRepsAndSet
                   |> required "repsAndSetName" string
                   |> required "repsAndSetReps" int
                   |> required "repsAndSetRestTime" int
                   |> required "repsAndSetSets" int



timeAndSetsDecoder : Decoder Exercise
timeAndSetsDecoder = Decode.succeed createTimeAndSet
                   |> required "timeAndSetName" string
                   |> required "timeAndSetActiveTime" int
                   |> required "timeAndSetRestTime" int
                   |> required "timeAndSetSets" int


complexDecoder : Decoder Exercise
complexDecoder = Decode.succeed createComplex
                 |> required "complexName" string
                 |> required "complexSets" int
                 |> required "complexRestTime"  int
                 |> required "complexRoundsPerSet" int
                 |> required "complexExercises" ( list (Decode.lazy (\_ -> exerciseDecoder)))



exerciseDecoder : Decoder Exercise
exerciseDecoder = atdDecoder [(restDecoder,"Rest"), (repsAndSetsDecoder,"RepsAndSet"), (complexDecoder,"Complex"), (timeAndSetsDecoder,"TimeAndSet")]




-- ROUTINE

routinesInfoDecoder : Decoder RoutineList
routinesInfoDecoder = list routineInfoDecoder

routineInfoDecoder : Decoder RoutineInfo
routineInfoDecoder = Decode.succeed createRoutineInfo
                     |> required "infoId" int
                     |> required "infoName" string

routineDecoder : Decoder Routine
routineDecoder = Decode.succeed createRoutine
                 |> required "routineId" int
                 |> required "routineName" string
                 |> required "routineExercises" (list exerciseDecoder)



-- ERRORS

getErrorString : Decode.Error -> String
getErrorString err = Decode.errorToString err
