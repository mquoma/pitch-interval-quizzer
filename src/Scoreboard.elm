-- module PitchIntervalQuizzer exposing (..)

-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)


-- -- Model


-- type alias Model =
--     { score : Int
--     }


-- initModel : ( Model, Cmd Msg )
-- initModel =
--     ( { toneArray = toneArray
--       , playStatus = False
--       , pick = 1
--       , userPick = 0
--       , displayMessage = ""
--       , score = 0
--       }
--     , Cmd.none
--     )



-- -- UPDATE


-- type Msg
--     = Play
--     | SetUserGuess Int
--     | RequestNewTones
--     | SetNewTones Int



-- -- VIEW


-- view : Model -> Html Msg
-- view model =
--     let
--         url =
--             "audio/" ++ (toString (model.pick)) ++ ".mp3"
--     in
--         div [ class "scoreboard" ]
--             [ h1 [] [ text "Pitch Interval Quizzer v1.01" ]
--             , audio
--                 [ src url
--                 , controls True
--                 , autoplay True
--                 ]
--                 []
--             , answers model
--             , button
--                 [ type_ "button"
--                 , onClick (RequestNewTones)
--                 ]
--                 [ text "Next" ]
--             , div [] [ text (model.displayMessage) ]
--             , div [] [ text ("Score:" ++ (toString model.score)) ]
--             ]


-- displayPick : Model -> Int -> Html Msg
-- displayPick model p =
--     let
--         member =
--             (get p (model.toneArray))

--         tuple =
--             case member of
--                 Nothing ->
--                     ( 0, ( ' ', ' ' ) )

--                 Just a ->
--                     a

--         ( id, tones ) =
--             tuple

--         ( low, high ) =
--             tones
--     in
--         div []
--             [ div [] [ text (toString id) ]
--             , div [] [ text (toString low) ]
--             , div [] [ text (toString high) ]
--             ]


-- answer : Int -> Html Msg
-- answer a =
--     span [ style [] ]
--         [ button [ onClick (SetUserGuess a) ]
--             [ text (toString a) ]
--         ]


-- answers : Model -> Html Msg
-- answers model =
--     let
--         tones =
--             toList model.toneArray
--     in
--         List.map
--             (\c ->
--                 let
--                     ( id, tones ) =
--                         c
--                 in
--                     answer id
--             )
--             tones
--             |> div []



-- -- subscriptions


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Sub.none


-- main : Program Never Model Msg
-- main =
--     Html.program
--         { init = initModel
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
