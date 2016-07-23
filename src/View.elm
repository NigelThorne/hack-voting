module View exposing (root)

import Room.View
import Exts.Html.Bootstrap exposing (..)
import Exts.RemoteData exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ container
            [ h1 [] [ text "Vote-o-matic" ]
            , button
                [ class "btn btn-primary pull-right"
                , onClick Authenticate
                , disabled (model.auth /= NotAsked)
                ]
                [ text "Log In" ]
            , case model.auth of
                Success user ->
                    case model.roomModel of
                        Nothing ->
                            text "Initialising."

                        Just roomModel ->
                            Room.View.root user roomModel
                                |> Html.map RoomMsg

                Failure err ->
                    div [ class "alert alert-danger" ] [ text err.message ]

                Loading ->
                    h2 [] [ i [] [ text "Checking your account." ] ]

                NotAsked ->
                    h2 [] [ text "Log in to view and vote." ]
            ]
        ]
