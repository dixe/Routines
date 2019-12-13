module SmartJson exposing (atdDecoder)

import Json.Decode as Decode exposing (Decoder, Error(..), field, map, map2, map3, map4, int, string, decodeString, errorToString, list, oneOf, decodeValue, succeed, andThen, fail) -- maybe not in type module
import Json.Decode.Pipeline exposing (required, optional, hardcoded)



-- UTILITY FUNCTIONS
intDecoder : String -> Decoder Int
intDecoder n = field n int

stringFieldDecoder : String -> Decoder String
stringFieldDecoder n = field n string


--ATD DECODERS maybe allow the tag to be specified


atdDecoderHelper : List( (Decoder a, String)) -> String -> Decoder a
atdDecoderHelper decoders tag =
    oneOf (List.map (\(decoder, target) -> applyTag tag  target decoder) decoders)


applyTag : String -> String -> Decoder a -> Decoder a
applyTag tag target decoder =
    atdTagDecoder tag target decoder


atdDecoder : (List (Decoder a, String)) -> Decoder a
atdDecoder decoders = stringFieldDecoder "tag"
                      |> andThen (atdDecoderHelper decoders)


atdTagDecoder : String -> String -> Decoder a -> Decoder a
atdTagDecoder tag target decoder =
    if tag == target
    then decoder
    else fail ("Fail to decode tag '" ++ tag ++ "' as " ++ target)
