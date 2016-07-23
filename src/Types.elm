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
    { auth : RemoteData Firebase.Error Firebase.User
    , roomModel : Maybe Room.Types.Model
    }
