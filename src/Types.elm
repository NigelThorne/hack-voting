module Types exposing (..)

import Room.Types
import Exts.RemoteData exposing (..)
import Firebase.Auth as Firebase
import Firebase.Common as Firebase


type Msg
    = Authenticate
    | AuthResponse (RemoteData Firebase.Error Firebase.User)
    | RoomMsg Room.Types.Msg


type alias Model =
    { firebase : Firebase.AuthData
    , room : Maybe Room.Types.Model
    }
