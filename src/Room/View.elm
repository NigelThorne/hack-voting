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
import String exposing (lines)

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
            
        roomTopic = 
            room.topic
                |> Maybe.withDefault "..pick something to vote on." 

        rowsCount = roomTopic |> String.lines |> List.length
    in
        div []
            [ h3 [] [text "Hi "
                    , input [class "", placeholder "Your name goes here", onInput ChangeName, value userName] []
                    ]
            , h4 [] [text "How big is: "]
            , textarea [class "topic", value roomTopic, onInput ChangeTopic, rows rowsCount, wrap "hard"] []
            , div [class "col-12"]
                  [ deckView userVote model ]
            , div [class "col-12"]
                  [ well [ votesView room ]]
            ]

deckView : Vote -> Model -> Html Msg
deckView userVote model =
    case model.deck of
        Success deck ->
            div []
                ((h3 [] [text "Pick one: "])  :: (deck |> List.map (cardView userVote)))

        Failure err ->
            div [ class "alert alert-danger" ] [ text err ]

        Loading ->
            h2 [] [ i [] [ text "Waiting for deck data..." ] ]

        NotAsked ->
            h2 [] [ text "Initialising Deck." ]


cardView : Vote -> Card  -> Html Msg
cardView userVote card =
    voteButtons userVote card

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

voters : Dict UserId Name -> UserId -> Name
voters names id = 
    Dict.get id names |> Maybe.withDefault "Anonymous"

collectNames : Dict UserId Name -> UserId -> Vote -> Dict Card (List Name) -> Dict Card (List Name)
collectNames names uid vote dict =
    case vote of 
        Just card -> 
            case Dict.get uid names of
                Just name -> 
                    let
                        array = case (Dict.get card dict) of 
                            Just vals -> vals
                            Nothing -> []
                    in
                        Dict.insert card (name :: array) dict
                Nothing -> dict
        Nothing -> dict

votesView : Room -> Html Msg
votesView room =
    let
        voteCounts =
            tally room.votes

        voterNames = 
            room.votes 
                |> Dict.filter (\k v -> v /= Nothing)
                |> Dict.keys
                |> List.map (voters room.voters)

        maxCount =
            voteCounts
                |> Dict.values
                |> List.maximum
                |> Maybe.withDefault 0

        voterspercard = 
            Dict.foldl (collectNames room.voters) Dict.empty room.votes 

        tallied =
            tally room.votes
                |> Dict.toList

        totalCount = voteCounts
                |> Dict.values
                |> List.foldl (\v acc-> v + acc) 0

        showVotes = room.showVotes
    in
        div [classList [("votes", True)]] (List.append ( 
                            if showVotes then
                                tallied
                                    |> List.map (voteBar voterspercard maxCount )
                                    |> List.intersperse (hr [] [])
                            else
                                [ h2 [] [ text ((toString totalCount) ++ " votes cast ... ") ]
                                , ul [] (voterNames 
                                        |> List.map (\t -> li [] [text t]) )] 
                        )
                        [voteShowToggleButton room.showVotes]  
                )
            

voteShowToggleButton : Bool -> Html Msg
voteShowToggleButton showing =
    button
        [ classList
            [ ( "btn", True )
            ]
        , onClick
            ( RevealResults (not showing) )
        ]
        [ if showing then  text "Hide votes" else text "Press when *Everyone* has finished voting!" ]


--- results view
voteBar :  Dict Card (List Name) -> Int -> ( Card, Int )  -> Html msg
voteBar  names maxCount ( card, voteCount ) =
    let

        width =
            (toFloat voteCount / toFloat maxCount)
                * 100.0

        pct n =
            toString n ++ "%"

        voters = (case Dict.get card names of
                Just all -> all
                Nothing -> [])
            |> List.map text
            |> List.intersperse (text ", ")
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
                voters
            ]

badge : Int -> Html msg
badge n =
    span [ class "badge" ] [ text (toString n) ]
