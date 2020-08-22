port module PitchIntervalQuizzer exposing (..)

import Browser exposing (element)
import Html exposing (Html, div, h4, text, button, audio, li, span)
import Html.Attributes exposing (class, type_, autoplay, controls, src)
import Html.Events exposing (onClick)
import Array exposing (..)
import Random


type alias Model =
    { toneArray : Array Tone
    , pick : Int
    , userPick : Int
    , displayMessage : String
    , score : Int
    }

type alias Tone =
    { index : Int
    , tones : (Char, Char)
    , label : String
    }

toneArray =
    Array.fromList
        [ Tone 1 ( 'C', 'C' ) "Octave" 
        , Tone 2 ( 'C', 'D' ) "Second" 
        , Tone 3 ( 'C', 'E' ) "Major Third" 
        , Tone 4 ( 'C', 'F' ) "Fourth" 
        , Tone 5 ( 'C', 'G' ) "Fifth" 
        , Tone 6 ( 'C', 'A' ) "Major Sixth" 
        , Tone 7 ( 'C', 'B' ) "Major Seventh" 
        , Tone 8 ( 'D', 'F' ) "Minor Third" 
        , Tone 9 ( 'E', 'C' ) "Minor Sixth"
        , Tone 10 ( 'F', 'B' ) "TRITONE!" 
        ]

init: () -> (Model, Cmd msg)
init i =
    ( { toneArray = toneArray
      , pick = 1
      , userPick = 0
      , displayMessage = ""
      , score = 0
      }
    , Cmd.none
    )



type Msg
    = SetUserGuess Int
    | RequestNewInterval
    | SetNewInterval Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SetUserGuess guess ->
            if guess == model.pick then

                    ( 
                        { model | userPick = guess, displayMessage = "YES.", score = model.score + 1 }, 
                        Cmd.none 
                    )
                else

                    ( 
                        { model | userPick = guess, displayMessage = "NO.", score = model.score - 1 }, 
                        Cmd.none
                    )

        RequestNewInterval ->
            ( model, generateRandomInterval )

        SetNewInterval interval ->
            ( { model | pick = interval, userPick = 0, displayMessage = "" }, sineWave (1.0, 7/4) )



generateRandomInterval : Cmd Msg
generateRandomInterval =
    Random.int 1 (length toneArray)
    |> Random.generate SetNewInterval 


view : Model -> Html Msg
view model =
    let
        url =
            "audio/" ++ (String.fromInt (model.pick)) ++ ".mp3"
    in
        div [ class "scoreboard" ]
            [ h4 [] [ text "Pitch Interval Quizzer v1.02" ]
            , audio
                [ src url
                , controls True
                , autoplay True
                ]
                []
              --            , div [] [ displayPick model model.pick ]
              --            , div [] [ text (toString model.pick) ]
              --, div [] [ text (toString model.userPick) ]
            , renderAnswers model
            , button
                [ type_ "button"
                , onClick RequestNewInterval
                ]
                [ text "NEXT MOVE" ]
            , div [] [ text model.displayMessage]
            , div [] [ text ("Score: " ++ String.fromInt model.score) ]
            ]


displayPick : Model -> Int -> Html Msg
displayPick model p =
    let
        member =
            (get p (model.toneArray))
            |> Maybe.withDefault (Tone 0 ( ' ', ' ' ) "" )

        ( low, high ) =
            member.tones
    in
        div []
            [ div [] [ text (String.fromInt member.index) ]
            , div [] [ text ( String.fromChar low) ]
            , div [] [ text ( String.fromChar high) ]
            ]

renderAnswer : Tone -> Html Msg
renderAnswer {index, label} =
    span []
        [ button [ onClick (SetUserGuess index) ]
            [ text label ]
        ]


renderAnswers : Model -> Html Msg
renderAnswers model =

    model.toneArray
    |> toList
    |> List.map renderAnswer 
    |> div []



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
  Browser.element 
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


port sineWave : (Float, Float) -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg
