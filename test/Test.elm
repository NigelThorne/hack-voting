module Test exposing (main)

{-| The main entry point for the tests.

@docs main
-}

import ElmTest exposing (..)
import StateTest
--import Console

tests : Test
tests =
    suite "All"
        [ StateTest.tests ]


{-| Run the test suite under node.
-}
main : Program Never
main =
    runSuite tests


--port runner : Signal (Task.Task x ())
--port runner =
--    Console.run (consoleRunner tests)
