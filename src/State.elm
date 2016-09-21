module State exposing (..)

import Room.State as Room
import Exts.RemoteData as RemoteData exposing (..)
import Firebase.Auth as Firebase
import Response exposing (..)
import Types exposing (..)


initialState : ( Model, Cmd Msg )
initialState =
    ( { firebase = Loading
      , room = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , model.room
            |> Maybe.map (Room.subscriptions >> Sub.map RoomMsg)
            |> Maybe.withDefault Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( { model | firebase = Loading }
            , Firebase.authenticate ()
            )

        AuthResponse response ->
            let
                newModel =
                    { model | firebase = response }
            in
                Room.initialState
                    |> mapModel (\room -> { newModel | room = Just room })
                    |> mapCmd RoomMsg

        RoomMsg submsg ->
            case ( model.firebase, model.room ) of
                ( Success user, Just room ) ->
                    Room.update user submsg room
                        |> mapModel (\room -> { model | room = Just room })
                        |> mapCmd RoomMsg

                _ ->
                    ( model, Cmd.none )
