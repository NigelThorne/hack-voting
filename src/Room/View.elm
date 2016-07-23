module Room.View exposing (root)

import Dict exposing (Dict)
import Room.State exposing (..)
import Room.Types exposing (..)
import Exts.Html.Bootstrap exposing (..)
import Exts.RemoteData exposing (..)
import Firebase.Auth exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- dealing with authorization 
root : User -> Model -> Html Msg
root user model =
    case model.room of
        Success room ->
            roomView user room model

        Failure err ->
            div [ class "alert alert-danger" ] [ text err ]

        Loading ->
            h2 [] [ i [] [ text "Waiting for room data..." ] ]

        NotAsked ->
            h2 [] [ text "Initialising Room." ]


roomView : User -> Room -> Model -> Html Msg
roomView user room model =
    let
        userVote =
            Dict.get user.uid room.votes
                |> Maybe.withDefault initialVote

        userName =
            Dict.get user.uid room.voters
                |> Maybe.withDefault initialName

        roomTopic = case room.topic of
            Nothing -> "..pick something to vote on."
            Just t -> t

    in
        div []
            [ input [class "col-xs-12 topic", value roomTopic, onInput ChangeTopic] [] 
            , yourVote userVote
            , row
                [ div [ class "col-xs-12 col-sm-6" ]
                    [ deckView userVote model ]
                , div [ class "col-xs-12 col-sm-6" ]
                    [ votesView room ]
                ]
            ]


yourVote : Vote -> Html msg
yourVote userVote =
    h3 [class "col-xs-12"]
        [ case (userVote) of
            (Just _)  ->
                text "Thanks for voting!"

            _ ->
                text "Please use your remaining votes."
        ]


deckView : Vote -> Model -> Html Msg
deckView userVote model =
    case model.deck of
        Success deck ->
            div []
                [ h2 [] [ text "Estimate" ]
                , div [ class "list-group" ]
                    (deck |> List.map (cardView userVote))
                ]

        Failure err ->
            div [ class "alert alert-danger" ] [ text err ]

        Loading ->
            h2 [] [ i [] [ text "Waiting for deck data..." ] ]

        NotAsked ->
            h2 [] [ text "Initialising Deck." ]


cardView : Vote -> Card  -> Html Msg
cardView userVote card =
    div [ class "list-group-item" ]
        [ div [ class "pull-right" ]
            [ voteButtons userVote card ]
        , h3 [] [ text card ]
        ]


voteButtons : Vote -> Card  -> Html Msg
voteButtons vote card  =
    let
        ordButton  =
            let
                active =
                    case vote of
                        Nothing ->
                            False

                        Just votedCard ->
                            votedCard == card
            in
                button
                    [ classList
                        [ ( "btn", True )
                        , ( "btn-default", not active )
                        , ( "btn-info", active )
                        ]
                    , onClick
                        (VoteFor 
                            (if active then
                                Nothing
                             else
                                Just card
                            )
                        )
                    ]
                    [ text card ]
    in
        div [ class "btn-group" ]
            [ ordButton ]


tally : Dict String Vote -> Dict Card Int
tally votes =
    let
        increment =
            Just << (+) 1 << Maybe.withDefault 0
    in
        List.foldl
            (\vote acc ->
                case vote of
                    Just card -> Dict.update card increment acc
                    Nothing -> acc                 
            )
            Dict.empty
            (Dict.values votes)


votesView : Room -> Html Msg
votesView room =
    let
        voteCounts =
            tally room.votes

        maxCount =
            voteCounts
                |> Dict.values
                |> List.maximum
                |> Maybe.withDefault 0

        tallied =
            tally room.votes
                |> Dict.toList

        totalCount = voteCounts
                |> Dict.values
                |> List.foldl (\v acc-> v + acc) 0

        showVotes = room.showVotes
    in
        div []
            [ h2 [] [ text ((toString totalCount) ++" Votes") ]
            ,   if showVotes then
                    if List.isEmpty tallied then
                        empty
                    else
                        well
                            (tallied
                                |> List.map (voteBar maxCount)
                                |> List.intersperse (hr [] [])
                            )
                else
                    div [] [text "Voting in progress ..."]
            , voteShowToggleButton room.showVotes
            , div [] [
                    whoVotedWhat room.votes
                ]
            ]

whoVotedWhat : Dict String Vote -> Html Msg
whoVotedWhat votes = 
    h3 [] []

voteShowToggleButton : Bool -> Html Msg
voteShowToggleButton showing =
    button
        [ classList
            [ ( "btn", True )
            ]
        , onClick
            ( RevealResults (not showing) )
        ]
        [ text "Toggle Results" ]

voteBar :  Int -> ( Card, Int ) -> Html msg
voteBar  maxCount ( card, voteCount ) =
    let

        width =
            (toFloat voteCount / toFloat maxCount)
                * 100.0

        pct n =
            toString n ++ "%"
    in
        div []
            [ h3 []
                [ text card
                , text " "
                , badge voteCount
                ]
            , div
                [ style
                    [ ( "width", pct width )
                    , ( "margin", "15px 0" )
                    , ( "padding", "10px" )
                    , ( "background-color", "#3DF236" )
                    , ( "border", "solid 2px #28A024" )
                    , ( "border-radius", "10px" )
                    , ( "transition", "width 200ms" )
                    ]
                ]
                []
            ]


badge : Int -> Html msg
badge n =
    span [ class "badge" ] [ text (toString n) ]
