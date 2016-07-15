module App exposing (main)

{-| The main entry point for the app.

ok.. I want to change this to be a "story estimation tool"

Usage:  
    Everyone visits the page (gets anon auth). (by room name)
    Everyone types in their name.
    Anyone presses "new card" to clear the card and estimates.
    Anyone types in the story name.
    Everyone starts in the "?" state 
    Anyone can ask a question
    Anyone can answer.
    Everyone votes on the size. -- votes not shown, but status changes to "voted"
        
        Once everyone has voted... 
    Anyone can press the button to "show votes"

@docs main
-}

import Html.App
import State
import View


{-| Run the application.
-}
main : Program Never
main =
    Html.App.program
        { init = State.initialState
        , subscriptions = State.subscriptions
        , update = State.update
        , view = View.root
        }
