module Exercise exposing (Exercise, getExerciseName, viewRoutineExercises, createRepsAndSet, createRest, createComplex, updateTick, viewRunExercise, finishSet, createTimeAndSet)


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Message exposing(..)
import LayoutHelpers exposing(..)

-- atm just name, maybe we also want description
type Exercise = Rest Int
              | TimeAndSet { activeTime : Int
                           , name : String
                           , sets : Int
                           , restTime : Int
                           , currentSet : Int
                           , resting : Bool
                           , rested : Int
                           }

              | RepsAndSet { reps : Int
                           , name : String
                           , sets : Int
                           , restTime : Int
                           , currentSet : Int
                           , resting : Bool
                           , rested : Int
                           }

              | Complex { name : String
                        , sets : Int
                        , restTime : Int
                        , roundsPerSet : Int
                        , exercises : List Exercise
                        , currentSet : Int
                        , resting : Bool
                        , rested : Int
                        }


type alias BaseExercise a = { a | name : String
                            , sets : Int
                            , restTime : Int
                            , currentSet : Int
                            , resting : Bool
                            , rested : Int
                            }

-- GET AND UPDATE
getExerciseName : Exercise -> String
getExerciseName ex =
    case ex of
        Rest time -> "Rest: " ++ String.fromInt time
        RepsAndSet e -> "Reps and set: " ++ e.name
        TimeAndSet tas -> "Time and sets: " ++ tas.name
        Complex c -> "Complex: " ++ c.name


getExerciseTime : Exercise -> Maybe Int
getExerciseTime e =
    case e of
        Rest t -> Just t
        RepsAndSet _ -> Nothing
        TimeAndSet tas -> Just tas.activeTime
        Complex c -> Just c.restTime


-- VIEW


viewRoutineExercises : List Exercise -> Element Msg
viewRoutineExercises es = column [spacing 10] (List.map2 viewExercise (addSubNumbers [] (List.length es))  es)

viewExercise : List Int -> Exercise -> Element Msg
viewExercise index e =
    column []
        [
         case e of
             Rest time -> showExerciseName index <| "Rest - " ++ String.fromInt time
             TimeAndSet t -> showExerciseName index <| "TaS - " ++ t.name
             RepsAndSet r -> showExerciseName index <| "RaS - " ++ r.name
             Complex c -> column [spacing 10]
                          [ showExerciseName index <| "Complex - " ++ c.name
                          , column [spacing 10,  paddingXY 10 0] <| List.map2 viewExercise (addSubNumbers index ( List.length c.exercises))  c.exercises
                          ]
        ]



viewRunExercise : Exercise -> Int -> Element Msg
viewRunExercise e elapsed=
    case e of
        Rest t -> text <| "Rest time: " ++ offString elapsed t
        TimeAndSet tas -> text <| "time and set " ++ offString tas.currentSet tas.sets
        RepsAndSet ras -> viewRepsAndSet <| RepsAndSet ras
        Complex c -> text "Complex"



offString : Int -> Int -> String
offString current target = String.fromInt current ++ "/" ++ String.fromInt target


viewRepsAndSet : Exercise -> Element Msg
viewRepsAndSet e =
    case e of
        RepsAndSet ras ->
            let resting =
                    case ras.resting of
                        True -> text <| "REST! " ++ offString ras.rested ras.restTime
                        False -> column [] [ text "Go power go"
                                           , Input.button
                                               buttonLayout
                                               { onPress = Just FinishSet
                                               , label = text "Set done"
                                               }
                                           ]
            in
                column [] [text <| ras.name ++ " " ++ offString ras.currentSet ras.sets
                          , resting
                          ]
        _ -> none


-- VIEW HELPERS

addSubNumbers : List Int -> Int -> List (List Int)
addSubNumbers index len = List.map (\x-> index ++ [x]) (List.range 1 len)

     -- given [1,3] 5 produce [[1,3,1],[1,3,2], [1,3,3],[1,3,4],[1,3,5]]
                           --(List.range 1 (List.length c.exercises)
showExerciseName : List Int -> String -> Element Msg
showExerciseName  index name = text ((showIndex index) ++ ": " ++ name)

showIndex : List Int -> String
showIndex index = foldlDefault (\acc x -> x ++ "." ++ acc) "" (List.map String.fromInt index) -- Why is it reversed x ++ acc and not acc ++ x???


--UTILITY --TODO move to list util

foldlDefault : (a->a->a) -> a -> List a -> a
foldlDefault f default l = case l of
                               [] -> default
                               (x::xs) -> List.foldl f x xs


-- CREATERS

createRepsAndSet : String -> Int -> Int -> Int -> Exercise
createRepsAndSet name reps restTime sets = RepsAndSet {name = name, reps = reps, restTime = restTime, sets = sets, currentSet = 1, resting = False, rested = 0}

createTimeAndSet : String -> Int -> Int -> Int -> Exercise
createTimeAndSet name activeTime restTime sets = TimeAndSet {name = name, activeTime = activeTime, restTime = restTime, sets = sets, currentSet = 1, resting = False, rested = 0}


createRest : Int -> Exercise
createRest time = Rest time


createComplex : String -> Int -> Int -> Int -> (List Exercise) ->  Exercise
createComplex name sets restTime rounds exercises = Complex { name=name
                                                            , sets = sets
                                                            , restTime = restTime
                                                            , roundsPerSet = rounds
                                                            , exercises = exercises
                                                            , currentSet = 1
                                                            , resting = False
                                                            , rested = 0
                                                            }


updateTick : Int -> Exercise -> Maybe Exercise
updateTick elapsed ex =
    let updated =
            case ex of
                Rest _ -> ex
                TimeAndSet tas -> TimeAndSet <| updateSetBase tas
                RepsAndSet ras -> RepsAndSet <| updateSetBase ras
                Complex c -> Complex <| updateSetBase c
    in
        case updated of
            Rest t -> if elapsed >= t then Nothing else Just <| Rest t
            TimeAndSet tas -> if elapsed >= tas.activeTime then Nothing else Just <| TimeAndSet tas
            RepsAndSet ras -> if ras.currentSet >= ras.sets then Nothing else Just  <| RepsAndSet ras
            Complex _ -> Nothing


{-
applyBaseFunction : Exercise -> (BaseExercise a -> BaseExercise a) -> Exercise
applyBaseFunction ex f =
    let base =
            case ex of
                TimeAndSet tas -> TimeAndSet <|  {tas | f tas} --setRest tas | currentSet = tas.currentSet + 1 }
                RepsAndSet ras ->  finishSetBase ras
                Complex c -> finishSetBase c

    in
        base
-}


finishSet : Maybe Exercise -> Maybe Exercise
finishSet me =
    case me of
        Just e ->
            let ex =
                    case e of
                        Rest _ -> e
                        TimeAndSet tas -> TimeAndSet <| finishSetBase tas --setRest tas | currentSet = tas.currentSet + 1 }
                        RepsAndSet ras -> RepsAndSet <| finishSetBase ras
                        Complex c -> Complex <| finishSetBase c
            in Just ex
        Nothing -> me





updateSetBase : BaseExercise a -> BaseExercise a
updateSetBase be =
    case be.resting of
        False -> be
        True -> if be.rested >= be.restTime
                then { be | resting = False, rested = 0 }
                else { be | rested = be.rested + 1 }


finishSetBase : BaseExercise a -> BaseExercise a
finishSetBase be = { be | resting = True
                   , currentSet = be.currentSet + 1
                   , rested = 0
                   }
