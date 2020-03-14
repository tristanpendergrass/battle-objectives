module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Random
import Random.List


exclusiveNumbersGenerator : Int -> Int -> Random.Generator (List Int)
exclusiveNumbersGenerator min max =
    Random.map
        (List.take 4)
        (Random.List.shuffle (List.range min max))


seedFromInput : String -> Random.Seed
seedFromInput str =
    str
        |> String.toList
        |> List.map Char.toCode
        |> List.sum
        |> Random.initialSeed


type SeedInput
    = Empty
    | SeedValue String


type alias Model =
    { seedInput : SeedInput
    }


initialModel : Model
initialModel =
    { seedInput = Empty }


type Msg
    = HandleSeedInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleSeedInput "" ->
            { model | seedInput = Empty }

        HandleSeedInput str ->
            { model | seedInput = SeedValue str }


renderPlayer : Int -> Html Msg
renderPlayer number =
    div [] [ text ("Random number: " ++ String.fromInt number) ]


renderPlayers : String -> List (Html Msg)
renderPlayers seedValue =
    let
        numbers : List Int
        numbers =
            seedValue
                |> seedFromInput
                |> Random.step (exclusiveNumbersGenerator 1 10)
                |> Tuple.first
    in
    case numbers of
        [ first, second, third, fourth ] ->
            [ renderPlayer first
            , renderPlayer second
            , renderPlayer third
            , renderPlayer fourth
            ]

        _ ->
            [ div [] [ text "Something went wrong." ] ]


renderPlayersContainer : SeedInput -> Html Msg
renderPlayersContainer seedInput =
    case seedInput of
        Empty ->
            div [] [ text "Empty seed" ]

        SeedValue seedValue ->
            div [] (renderPlayers seedValue)


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput HandleSeedInput, placeholder "Enter seed" ] [ text "" ]
        , renderPlayersContainer model.seedInput
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
