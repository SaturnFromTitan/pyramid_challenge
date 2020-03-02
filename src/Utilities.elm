module Utilities exposing (secondsToTime, sumOf1To)

import String


sumOf1To : Int -> Int
sumOf1To n =
    -- Gaussian sum formula
    n * (n + 1) // 2


as2DigitString : Int -> String
as2DigitString num =
    let
        numAsString =
            String.fromInt num
    in
    if String.length numAsString == 1 then
        "0" ++ numAsString

    else
        numAsString


secondsToTime : Int -> String
secondsToTime totalSeconds =
    let
        hours =
            totalSeconds // 3600

        remainder =
            remainderBy 3600 totalSeconds

        minutes =
            remainder // 60

        seconds =
            remainderBy 60 remainder
    in
    String.concat
        [ as2DigitString hours
        , ":"
        , as2DigitString minutes
        , ":"
        , as2DigitString seconds
        ]
