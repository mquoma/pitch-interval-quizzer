module PitchIntervalQuizzer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Random
import Scorecard


-- Model


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


type alias Model =
    { toneArray : Array ( Int, ( Char, Char ), String )
    , playStatus : Bool
    , pick : Int
    , userPick : Int
    , displayMessage : String
    , score : Int
    }


initModel : ( Model, Cmd Msg )
initModel =
    ( { toneArray = toneArray
      , playStatus = False
      , pick = 1
      , userPick = 0
      , displayMessage = ""
      , score = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Play
    | SetUserGuess Int
    | RequestNewTones
    | SetNewTones Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( model, Cmd.none )

        SetUserGuess guess ->
            let
                guessId =
                    guess

                msg =
                    if (guessId == model.pick) then
                        "yes!"
                    else
                        "no."

                points =
                    if (guessId == model.pick) then
                        1
                    else
                        -1
            in
                ( { model | userPick = guess, displayMessage = msg, score = model.score + points }, Cmd.none )

        RequestNewTones ->
            ( model, generateRandom )

        SetNewTones n ->
            ( { model | pick = n, userPick = 0, displayMessage = "" }, Cmd.none )


generateRandom : Cmd Msg
generateRandom =
    Random.generate SetNewTones (Random.int 1 ((length toneArray)))



-- VIEW


view : Model -> Html Msg
view model =
    let
        url =
            "audio/" ++ (toString (model.pick)) ++ ".mp3"
    in
        div [ class "scoreboard" ]
            [ h1 [] [ text "Pitch Interval Quizzer v1.01" ]
            , audio
                [ src url
                , controls True
                , autoplay True
                ]
                []
              --            , div [] [ displayPick model model.pick ]
              --            , div [] [ text (toString model.pick) ]
              --, div [] [ text (toString model.userPick) ]
            , answers model
            , button
                [ type_ "button"
                , onClick (RequestNewTones)
                ]
                [ text "Next" ]
            , div [] [ text (model.displayMessage) ]
            , div [] [ text ("Score: " ++ (toString model.score)) ]
            ]


displayPick : Model -> Int -> Html Msg
displayPick model p =
    let
        member =
            (get p (model.toneArray))

        tuple =
            case member of
                Nothing ->
                    ( 0, ( ' ', ' ' ), "" )

                Just a ->
                    a

        ( id, tones, label ) =
            tuple

        ( low, high ) =
            tones
    in
        div []
            [ div [] [ text (toString id) ]
            , div [] [ text (toString low) ]
            , div [] [ text (toString high) ]
            ]


tone : Int -> Html Msg
tone t =
    li []
        [ text (toString t)
        ]


answer : Int -> Html Msg
answer a =
    span [ style [] ]
        [ button [ onClick (SetUserGuess a) ]
            [ text (toString a) ]
        ]


answers : Model -> Html Msg
answers model =
    let
        tones =
            toList model.toneArray
    in
        List.map
            (\c ->
                let
                    ( id, tones, label ) =
                        c
                in
                    answer id
            )
            tones
            |> div []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = initModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
