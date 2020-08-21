module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Play =
    { id : Int
    , playerId : Int
    , playerName : String
    , points : Int
    }


type alias Player =
    { id : Int
    , playerName : String
    , points : Int
    }

init: () -> (Model, Cmd msg)
init i =
     { players = []
        , playerName = ""
        , playerId = Nothing
        , plays = []
        } |> noCmd



-- UPDATE


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


noCmd : Model -> (Model, Cmd msg)
noCmd model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Input name ->
            Debug.log "input updated: " { model | playerName = name } 
            |> noCmd


        Edit player ->
            { model
                | playerId = Just player.id
                , playerName = player.playerName
            }
            |> noCmd

        Score player pts ->
            let
                newPlayers =
                    List.map
                        (\p ->
                            if (p.id == player.id) then
                                { p | points = p.points + pts }
                            else
                                p
                        )
                        model.players

                newPlays =
                    { id = List.length model.plays
                    , playerId = player.id
                    , playerName = player.playerName
                    , points = pts
                    }
                        :: model.plays
            in
                { model
                    | players = newPlayers
                    , plays = newPlays
                }
                |> noCmd

        Cancel ->
            { model | playerName = "", playerId = Nothing } 
            |> noCmd

        Save ->
            (
                if (String.isEmpty model.playerName) then
                    model
                else
                    save model
            )
            |> noCmd

        DeletePlay p ->
            deletePlay model p
            |> noCmd


deletePlay : Model -> Play -> Model
deletePlay model p =
    let
        newPlays =
            model.plays
                |> List.filter
                    (\a ->
                        a.id /= p.id
                    )

        newPlayers =
            model.players
                |> List.map
                    (\player ->
                        if (player.id == p.playerId) then
                            { player | points = player.points - p.points }
                        else
                            player
                    )
    in
        { model | plays = newPlays, players = newPlayers }


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\p ->
                    if (p.id == id) then
                        { p | playerName = model.playerName }
                    else
                        p
                )
                model.players

        newPlays =
            List.map
                (\p ->
                    if (p.id == id) then
                        { p | playerName = model.playerName }
                    else
                        p
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , playerName = ""
        }


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.playerName 0

        newPlayers =
            player :: model.players
    in
        { model | players = newPlayers, playerName = "" }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Scorekeeper: " ]
        , playerSection model
        , playerForm model
        , playsHeader
        , plays model
          --, p [] [ text (toString model) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    --ul [] (List.map player model.players)
    model.players
        |> List.sortBy .playerName
        |> List.map (playa model.playerId)
        |> ul []


playsHeader : Html Msg
playsHeader =
    header []
        [ div [] [ text "Player" ]
        , div [] [ text "Points" ]
        ]


plays : Model -> Html Msg
plays model =
    model.plays
        |> List.map play
        |> ul []


play : Play -> Html Msg
play p =
    li []
        [ i
            [ class "remove"
            , onClick <| DeletePlay p
            ]
            []
        , div [] [ text p.playerName ]
        , div [] [ text <| String.fromInt p.points ]
        ]


playa : Maybe Int -> Player -> Html Msg
playa editId p =
    li []
        [ i
            [ class "edit"
            , onClick (Edit p)
            ]
            []
        , div [ class (editPlayerClass editId p) ]
            [ text p.playerName ]
        , button
            [ type_ "button"
            , onClick (Score p 2)
            ]
            [ text "2 Pts" ]
        , button
            [ type_ "button"
            , onClick (Score p 3)
            ]
            [ text "3 Pts" ]
        , div [] [ text <| String.fromInt p.points ]
        ]


editPlayerClass : Maybe Int -> Player -> String
editPlayerClass editId player =
    case editId of
        Just id ->
            if (player.id == id) then
                "edit"
            else
                ""

        Nothing ->
            ""


pointsTotal : Model -> Html Msg
pointsTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total" ]
            , div [] [ text <| String.fromInt total ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "input"
            , onInput Input
            , placeholder "Add/Edit Player.."
            , value model.playerName
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Clear" ]
        ]


main =
  Browser.element 
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
