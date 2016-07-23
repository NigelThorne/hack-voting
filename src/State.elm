module State exposing (..)

import Room.State as Room
import Exts.RemoteData as RemoteData exposing (..)
import Firebase.Auth as Firebase
import Response exposing (..)
import Types exposing (..)


initialState : ( Model, Cmd Msg )
initialState =
    ( { auth = Loading
      , roomModel = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AuthResponse Firebase.authResponse
        , model.roomModel
            |> Maybe.map (Room.subscriptions >> Sub.map RoomMsg)
            |> Maybe.withDefault Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( { model | auth = Loading }
            , Firebase.authenticate ()
            )

        AuthResponse response ->
            let
                newModel =
                    { model | auth = response }
            in
                Room.initialState
                    |> mapModel (\roomModel -> { newModel | roomModel = Just roomModel })
                    |> mapCmd RoomMsg

        RoomMsg submsg ->
            case ( model.auth, model.roomModel ) of
                ( Success user, Just roomModel ) ->
                    Room.update user submsg roomModel
                        |> mapModel (\roomModel -> { model | roomModel = Just roomModel })
                        |> mapCmd RoomMsg

                _ ->
                    ( model, Cmd.none )
