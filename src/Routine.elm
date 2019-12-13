module Routine exposing(Routine, RoutineListMaybe, RoutineList, RoutineInfo, getName, getId, getExercises, getNameInfo, getIdInfo, createRoutineInfo, createRoutine)

import Exercise exposing (..)

type Routine = Routine  { id : Int
                        , name : String
                        , exercises : (List Exercise)
                        }


type RoutineInfo = RoutineInfo { id : Int
                               , name : String
                               }


type alias RoutineList = List RoutineInfo

type alias RoutineListMaybe = Maybe RoutineList





-- GETTER AND SETTERS

getName : Routine -> String
getName (Routine r) = r.name

getId : Routine -> Int
getId (Routine r) = r.id

getExercises : Routine -> List Exercise
getExercises (Routine r) = r.exercises

getNameInfo : RoutineInfo -> String
getNameInfo (RoutineInfo r) = r.name

getIdInfo : RoutineInfo -> Int
getIdInfo (RoutineInfo r) = r.id


-- CREATE FUNCTIONS

createRoutine : Int -> String -> (List Exercise) -> Routine
createRoutine id name exs = Routine {id = id, name = name, exercises = exs }


createRoutineInfo : Int -> String -> RoutineInfo
createRoutineInfo id name = RoutineInfo { id = id, name = name }
