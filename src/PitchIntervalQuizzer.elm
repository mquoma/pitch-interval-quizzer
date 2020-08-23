port module PitchIntervalQuizzer exposing (..)

import Browser exposing (element)
import Html exposing (Html, div, h4, text, button, audio, li, span)
import Html.Attributes exposing (class, type_, value, for, name, checked, autoplay, controls, src)
import Html.Events exposing (onClick)
import Array exposing (..)
import Random

type TuningMode = Pure | Mean
type DifficultyLevel = Easy | Hard

type alias Tone =
    { tones : (Char, Char)
    , ratio : Float
    , intervalName : String
    }

type alias Model =
    { toneArray : Array Tone
    , actualAnswer : Tone
    , multipleChoices : List Tone
    , userAnswer : Float
    , displayMessage : String
    , score : Int
    , tuningMode : TuningMode
    , difficultyLevel : DifficultyLevel
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
      , actualAnswer = Tone (' ',' ') 0.0 ""
      , multipleChoices = []
      , userAnswer = 0.0
      , displayMessage = ""
      , score = 0
      , tuningMode = Mean
      , difficultyLevel = Easy

      }
    , Cmd.none
    )

type Msg
    = SetUserGuess Float
    | Replay Float Float
    | RequestNewInterval
    | SetNewInterval (Int, Int)
    | SetTuningMode TuningMode
    | SetDifficultyLevel DifficultyLevel
    | SetMultipleChoices (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SetMultipleChoices list ->

            let 
                tones = 
                    list
                        |> List.map (\a -> 
                            (get (a - 1) (model.toneArray)) 
                                |> Maybe.withDefault (Tone (' ', ' ') 0.0 ""))  
            in
            ( {model | multipleChoices = tones}, Cmd.none )

        SetTuningMode tuningMode ->
            if tuningMode == Pure then
                ({model | tuningMode = Pure, toneArray = pureTones}, Cmd.none)
            else
                ({model | tuningMode = Mean, toneArray = meanTones}, Cmd.none)
    

        SetDifficultyLevel difficultyLevel ->
            if difficultyLevel == Easy then
                ({model | difficultyLevel = Easy}, Cmd.none)
            else
                ({model | difficultyLevel = Hard}, Cmd.none)
    

        SetUserGuess guess ->
            if guess == model.actualAnswer.ratio then

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
            ( model, 
            Cmd.batch [
                generateRandomInterval (length model.toneArray),
                generateRandomMultipleChoices (length model.toneArray)
            ]
            )

        Replay offset ratio ->
            let 
                command = 
                    if model.difficultyLevel == Easy then
                        sineWave (1.0, ratio)      -- always use the same root note.
                    else 
                        sineWave (offset,  ratio)   -- offset the root note with random new note!
            in
            ( model, command )


        SetNewInterval (a, b) ->
            
            let 
        
                actual =
                    (get (b - 1) (model.toneArray))
                    |> Maybe.withDefault (Tone (' ', ' ') 0.0 "")

                root = 
                    (get (a - 1) (model.toneArray))
                    
                offset = 
                    case root of    
                        Nothing -> 
                            0.0
                        Just x ->
                            x.ratio
                
                command = 
                    if model.difficultyLevel == Easy then
                        sineWave (1.0, actual.ratio)
                    else 
                        sineWave (offset,  actual.ratio)
            in
            ( { model | actualAnswer = actual, userAnswer = 0.0, displayMessage = "" }, command )


--  Random.pair (Random.float -200 200) (Random.float -100 100)

generateRandomInterval : Int -> Cmd Msg
generateRandomInterval len =
    Random.pair (Random.int 1 len) (Random.int 1 len)
    |> Random.generate SetNewInterval 

generateRandomMultipleChoices len =
    Random.list 3 (Random.int 1 len)
    |> Random.generate SetMultipleChoices

view : Model -> Html Msg
view model =
        div [ class "scoreboard" ]
            [ h4 [] [ text "Pitch Interval Quizzer v1.03" ]
            , renderTuningMode model
            , renderDifficultyLevel model
            , renderMultpleChoices model
            , renderAnswers model
            , button
                [ type_ "button"
                , onClick RequestNewInterval
                ]
                [ text "NEXT" ]
            , button
                [ type_ "button"
                , onClick (Replay 1.0 model.actualAnswer.ratio)
                ]
                [ text "REPEAT" ]
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
        , Html.input [ type_ "radio", onClick (SetTuningMode Pure), name "tuningMode", value "pure", checked (model.tuningMode == Pure)] []
        , Html.label [for "tuningMode"] [text "Mean intonation"]
        , Html.input [ type_ "radio", onClick (SetTuningMode Mean), name "tuningMode", value "mean", checked (model.tuningMode == Mean)] []
    ]

renderDifficultyLevel : Model -> Html Msg
renderDifficultyLevel model = 
    div [] [
        Html.label [for "difficultyLevel"] [text "EASY"]
        , Html.input [ type_ "radio", onClick (SetDifficultyLevel Easy), name "difficultyLevel", value "easy", checked (model.difficultyLevel == Easy)] []
        , Html.label [for "difficultyLevel"] [text "HARD"]
        , Html.input [ type_ "radio", onClick (SetDifficultyLevel Hard), name "difficultyLevel", value "mean", checked (model.difficultyLevel == Hard)] []
    ]

renderMultpleChoices : Model -> Html Msg
renderMultpleChoices model =
    model.multipleChoices
        |> List.append [(model.actualAnswer)]
        -- |> List.sort
        |> List.map renderMultpleChoice
        |> Html.ul [] 

renderMultpleChoice : Tone -> Html Msg
renderMultpleChoice tone =
    Html.li [] [ text tone.intervalName]

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
