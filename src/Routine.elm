module Routine exposing(Routine, RoutineListMaybe, RoutineList, RoutineInfo, getName, getId, getExercises, getNameInfo, getIdInfo, createRoutineInfo, createRoutine, getRoutineList, createRoutineListMaybe, setFilterString, getRoutineListFiltered)

import Exercise exposing (..)

type Routine = Routine  { id : Int
                        , name : String
                        , exercises : (List Exercise)
                        }


type RoutineInfo = RoutineInfo { id : Int
                               , name : String
                               }


type alias RoutineList =  List RoutineInfo


type alias RoutineListMaybe = Maybe {routines : List RoutineInfo, nameFilter : String}



--HELPER

filterRoutine : String -> RoutineInfo -> Bool
filterRoutine s ri = String.contains (String.toLower s) (String.toLower (getNameInfo ri))



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


getRoutineListFiltered : RoutineListMaybe -> List RoutineInfo
getRoutineListFiltered rm =
    case rm of
        Nothing -> []
        Just ri -> List.filter (filterRoutine ri.nameFilter) ri.routines

getRoutineList : RoutineListMaybe -> List RoutineInfo
getRoutineList rm =
    case rm of
        Nothing -> []
        Just ri -> ri.routines


-- CREATE FUNCTIONS

createRoutine : Int -> String -> (List Exercise) -> Routine
createRoutine id name exs = Routine {id = id, name = name, exercises = exs }


createRoutineInfo : Int -> String -> RoutineInfo
createRoutineInfo id name = RoutineInfo { id = id, name = name }


createRoutineListMaybe : List RoutineInfo -> RoutineListMaybe
createRoutineListMaybe rl = Just  { routines = rl, nameFilter = ""}

-- UPDATE FUNCTIONS

setFilterString : String -> RoutineListMaybe -> RoutineListMaybe
setFilterString s rm =
    case rm of
        Just r -> Just {r | nameFilter = s}
        Nothing -> rm
