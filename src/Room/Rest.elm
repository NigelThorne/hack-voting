module Room.Rest exposing (..)
 
import Dict
import Exts.Maybe
import Room.Types exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode exposing (null)


encodeVote : Vote -> String
encodeVote vote = 
    Exts.Maybe.maybe Encode.null Encode.string vote
    |> Encode.encode 0

    --Encode.object
    --    [ ( "project", Exts.Maybe.maybe Encode.null Encode.string vote.project )
    --    , ( "name", Exts.Maybe.maybe Encode.null Encode.string vote.name )
    --    ]
    --    |> Encode.encode 0


decodeVote : Decoder Vote
decodeVote = maybe string

decodeVoter : Decoder Name
decodeVoter = string 

decodeCard : Decoder Card
decodeCard = string

decodeDeck : Decoder Deck
decodeDeck = list decodeCard

decodeRoom : Decoder Room
decodeRoom =
    decode Room
        |> optional "topic"     (maybe string) Nothing
        |> optional "votes"     (dict decodeVote) Dict.empty
        |> optional "voters"    (dict decodeVoter) Dict.empty
        |> optional "showVotes" ( bool ) False
