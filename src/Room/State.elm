module Room.State exposing (..)

import Dict
import Room.Ports exposing (..)
import Room.Rest exposing (..)
import Room.Types exposing (..)
import Exts.RemoteData as RemoteData exposing (..)
import Firebase.Auth exposing (User)
import Json.Decode as Decode


initialVote : Vote
initialVote = Nothing

initialName : Name
initialName = "Anonymous"

initialTopic : Topic
initialTopic = "Pick a Topic..."

initialRoom : Room
initialRoom =  
    { topic          = Just "Unknown Topic"
    , votes          = Dict.empty
    , voters         = Dict.empty
    , showVotes       = False
    }

initialState : ( Model, Cmd Msg )
initialState =
    ( { room = Loading
      , deck = Loading
      , roomError = Nothing
      , deckError = Nothing
      , voteError = Nothing
      }
    , roomListen ()
    )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ room (Decode.decodeString decodeRoom >> HeardRoom)
        , deck (Decode.decodeString decodeDeck >> HeardDeck)
        , roomError RoomError
        , deckError DeckError
        , voteSendError VoteError
        ]

update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        VoteError err ->
            ( { model | voteError = Just err }
            , Cmd.none
            )

        RoomError error ->
            ( { model | roomError = Just error }
            , Cmd.none
            )

        DeckError error ->
            ( { model | deckError = Just error }
            , Cmd.none
            )

        HeardRoom response ->
            ( { model | room = RemoteData.fromResult response }
            , Cmd.none
            )

        HeardDeck response ->
            ( { model | deck = RemoteData.fromResult response }
            , Cmd.none
            )

        VoteFor  card ->
            case model.room of
                Success room ->
                    ( model
                    , voteSend ( user.uid, card )
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeName name ->
            case model.room of
                Success room ->
                    ( model
                    , nameSend ( user.uid, name )
                    )

                _ ->
                    ( model, Cmd.none )

        RevealResults isRevealed ->
            case model.room of
                Success room ->
                    ( model
                    , votingCompleteSend (isRevealed)
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeTopic topic ->
            case model.room of 
                Success room -> 
                    ( model
                    , topicSend topic
                    )

                _ ->
                    ( model, Cmd.none )
