module PitchIntervalQuizzer exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    | RequestNewTones
    | SetNewTones Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SetUserGuess guess ->
            case guess == model.pick of
                True -> 
                    ( 
                        { model | userPick = guess, displayMessage = "YES.", score = model.score + 1 }, 
                        Cmd.none 
                    )
                _ -> 
                    ( 
                        { model | userPick = guess, displayMessage = "NO.", score = model.score - 1 }, 
                        Cmd.none 
                    )

        RequestNewTones ->
            ( model, generateRandom )

        SetNewTones n ->
            ( { model | pick = n, userPick = 0, displayMessage = "" }, Cmd.none )



generateRandom : Cmd Msg
generateRandom =
    Random.generate SetNewTones (Random.int 1 ((length toneArray)))
    |> Debug.log "this"



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
                , onClick (RequestNewTones)
                ]
                [ text "Next" ]
            , div [] [ text (model.displayMessage) ]
            , div [] [ text ("Score: " ++ (String.fromInt model.score)) ]
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


renderTone : Int -> Html Msg
renderTone t =
    li []
        [ text (String.fromInt t)
        ]


renderAnswer : Tone -> Html Msg
renderAnswer tone =
    span []
        [ button [ onClick (SetUserGuess tone.index) ]
            [ text tone.label ]
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
