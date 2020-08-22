port module PitchIntervalQuizzer exposing (..)

import Browser exposing (element)
import Html exposing (Html, div, h4, text, button, audio, li, span)
import Html.Attributes exposing (class, type_, value, for, name, checked, autoplay, controls, src)
import Html.Events exposing (onClick)
import Array exposing (..)
import Random

type TuningMode = Pure | Mean

type alias Model =
    { toneArray : Array Tone
    , actualAnswer : Float
    , userAnswer : Float
    , displayMessage : String
    , score : Int
    , tuningMode : TuningMode
    }

type alias Tone =
    { tones : (Char, Char)
    , ratio : Float
    , intervalName : String
    }

meanTones =
    Array.fromList
        [ Tone ( 'C', 'D' ) ( 2 ^ ( 1 / 12 ) ) "Minor Second"
        , Tone ( 'C', 'D' ) ( 2 ^ ( 2 / 12 ) ) "Major Second"
        , Tone ( 'D', 'F' ) ( 2 ^ ( 3 / 12 ) ) "Minor Third"
        , Tone ( 'C', 'E' ) ( 2 ^ ( 4 / 12 ) ) "Major Third"
        , Tone ( 'C', 'F' ) ( 2 ^ ( 5 / 12 ) ) "Fourth"
        , Tone ( 'F', 'B' ) ( 2 ^ ( 6 / 12 ) ) "TRITONE!"
        , Tone ( 'C', 'G' ) ( 2 ^ ( 7 / 12 ) ) "Fifth"
        , Tone ( 'E', 'C' ) ( 2 ^ ( 8 / 12 ) ) "Minor Sixth"
        , Tone ( 'C', 'A' ) ( 2 ^ ( 9 / 12 ) ) "Major Sixth"
        , Tone ( 'C', 'A' ) ( 2 ^ ( 10 / 12 ) ) "Minor Seventh"
        , Tone ( 'C', 'B' ) ( 2 ^ ( 11 / 12 ) ) "Major Seventh"
        , Tone ( 'C', 'C' ) ( 2 ^ ( 12 / 12 ) ) "Octave"
        ]

pureTones =
    Array.fromList
        [ Tone ( 'C', 'D' ) ( 9 / 8 ) "Minor Second" 
        , Tone ( 'C', 'D' ) ( 9 / 8 ) "Major Second" 
        , Tone ( 'D', 'F' ) ( 6 / 5 ) "Minor Third" 
        , Tone ( 'C', 'E' ) ( 5 / 4 ) "Major Third" 
        , Tone ( 'C', 'F' ) ( 4 / 3 ) "Fourth" 
        , Tone ( 'F', 'B' ) ( 45 / 32 ) "TRITONE!" 
        , Tone ( 'C', 'G' ) ( 3 / 2 ) "Fifth" 
        , Tone ( 'E', 'C' ) ( 8 / 5 ) "Minor Sixth"
        , Tone ( 'C', 'A' ) ( 5 / 3 ) "Major Sixth" 
        , Tone ( 'C', 'A' ) ( 5 / 3 ) "Minor Seventh" 
        , Tone ( 'C', 'B' ) ( 15 / 8 ) "Major Seventh" 
        , Tone ( 'C', 'C' ) ( 2 / 1) "Octave" 
        ]

init: () -> (Model, Cmd msg)
init i =
    ( { toneArray = meanTones
      , actualAnswer = 1 / 1
      , userAnswer = 0.0
      , displayMessage = ""
      , score = 0
      , tuningMode = Mean
      }
    , Cmd.none
    )



type Msg
    = SetUserGuess Float
    | RequestNewInterval
    | SetNewInterval Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SetUserGuess guess ->
            if guess == model.actualAnswer then

                    ( 
                        { model | displayMessage = "YES.", score = model.score + 1 }, 
                        Cmd.none 
                    )
                else

                    ( 
                        { model |  displayMessage = "NO.", score = model.score - 1 }, 
                        Cmd.none
                    )

        RequestNewInterval ->
            ( model, generateRandomInterval (length model.toneArray) )

        SetNewInterval interval ->
            
            let 
        
                tone =
                    (get (interval - 1) (model.toneArray))

                actual = 
                    case tone of    
                        Nothing -> 
                            0.0
                        Just x ->
                            x.ratio
                    

            in
            ( { model | actualAnswer = actual, userAnswer = 0.0, displayMessage = "" }, sineWave (1.0, actual) )



generateRandomInterval : Int -> Cmd Msg
generateRandomInterval len =
    Random.int 1 len
    |> Random.generate SetNewInterval 


view : Model -> Html Msg
view model =
        div [ class "scoreboard" ]
            [ h4 [] [ text "Pitch Interval Quizzer v1.03" ]
            , renderTuningMode model
            , renderAnswers model
            , button
                [ type_ "button"
                , onClick RequestNewInterval
                ]
                [ text "GO AGAIN!" ]
            , Html.h1 [] [ text model.displayMessage]
            , div [] [ text ("Score: " ++ String.fromInt model.score) ]
            ]


renderAnswer : Tone -> Html Msg
renderAnswer {ratio, intervalName} =
    span []
        [ button [ onClick (SetUserGuess ratio) ]
            [ text intervalName ]
        ]


renderTuningMode : Model -> Html Msg
renderTuningMode model = 
    div [] [
        Html.label [for "tuningMode"] [text "Pure intonation"]
        , Html.input [ type_ "radio", name "tuningMode", value "pure", checked (model.tuningMode == Pure)] []
        , Html.label [for "tuningMode"] [text "Mean intonation"]
        , Html.input [ type_ "radio", name "tuningMode", value "mean", checked (model.tuningMode == Mean)] []
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
