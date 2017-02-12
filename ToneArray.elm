module ToneArray exposing (..)

import Array exposing (..)


toneArray : Array ( Int, ( Char, Char ), String )
toneArray =
    Array.fromList
        [ ( 1, ( 'C', 'C' ), "Octave" )
        , ( 2, ( 'C', 'D' ), "Second" )
        , ( 3, ( 'C', 'E' ), "Major Third" )
        , ( 4, ( 'C', 'F' ), "Fourth" )
        , ( 5, ( 'C', 'G' ), "Fifth" )
        , ( 6, ( 'C', 'A' ), "Major Sixth" )
        , ( 7, ( 'C', 'B' ), "Major Seventh" )
        ]
