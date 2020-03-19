module Sound exposing (Sound(..), play)

import Ports


type Sound
    = Countdown


play : Sound -> Cmd msg
play sound =
    case sound of
        Countdown ->
            Ports.sounds "countdown"
