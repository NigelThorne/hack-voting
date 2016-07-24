module Room.Types exposing (..)

import Dict exposing (Dict)
import Exts.RemoteData exposing (..)
import Firebase.Common as Firebase


type alias Card =
    String

type alias Name =
    String

type alias Topic =
    String

type alias UserId =
    String

type alias Vote = 
    Maybe Card

type alias Deck = 
    List Card 

type alias Room = 
    { topic          : Maybe Topic
    , votes          : Dict UserId Vote
    , voters         : Dict UserId Name
    , showVotes      : Bool
    }

type alias Model =
    { room      : RemoteData String Room
    , deck      : RemoteData String Deck
    , roomError : Maybe Firebase.Error
    , deckError : Maybe Firebase.Error
    , voteError : Maybe Firebase.Error
    }

type Msg
    = HeardRoom (Result String Room)
    | HeardDeck (Result String Deck)
    | ChangeName Name
    | ChangeTopic Topic
    | RevealResults Bool
    | VoteFor Vote
    | RoomError Firebase.Error
    | DeckError Firebase.Error
    | VoteError Firebase.Error
