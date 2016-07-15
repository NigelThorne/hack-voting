module Event.Rest exposing (..)

import Dict
import Exts.Maybe
import Event.Types exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (null)


encodeVote : Vote -> String
encodeVote vote =
    Encode.object
        [ ( "first", Exts.Maybe.maybe Encode.null Encode.string vote.first )
        ]
        |> Encode.encode 0


decodeVote : Decoder Vote
decodeVote =
    decode Vote
        |> optional "first" (maybe string) Nothing


decodeProject : Decoder Project
decodeProject =
    decode Project
        |> required "name" string
        |> required "description" string


decodeEvent : Decoder Event
decodeEvent =
    decode Event
        |> optional "projects" (dict decodeProject) Dict.empty
        |> optional "votes" (dict decodeVote) Dict.empty
        |> required "showVotes" bool
