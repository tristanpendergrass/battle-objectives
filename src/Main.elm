module Main exposing (main)

import Browser
import Html exposing (Html, div, img, input, text)
import Html.Attributes exposing (class, placeholder, src)
import Html.Events exposing (onInput)
import Random
import Random.List


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


renderCard : String -> Html Msg
renderCard imageUrl =
    div [ class "flip-card" ]
        [ div [ class "flip-card-inner" ]
            [ div [ class "flip-card-back" ] [ img [ class "card", src imageUrl ] [] ]
            , div [ class "flip-card-front" ] [ img [ class "card", src "battle-goal-back.jpg" ] [] ]
            ]
        ]


renderPlayer : Int -> Int -> Int -> Html Msg
renderPlayer first second index =
    let
        imageUrl : Int -> String
        imageUrl number =
            String.fromInt number ++ ".jpg"
    in
    div []
        [ div [] [ text ("Player " ++ String.fromInt index) ]
        , renderCard (imageUrl first)
        , renderCard (imageUrl second)
        ]


renderPlayers : String -> List (Html Msg)
renderPlayers seedValue =
    let
        numbers : List Int
        numbers =
            seedValue
                |> seedFromInput
                |> Random.step (Random.List.shuffle (List.range 1 54))
                |> Tuple.first
                |> List.take 8
    in
    case numbers of
        [ first, second, third, fourth, fifth, sixth, seventh, eigth ] ->
            [ renderPlayer first second 1
            , renderPlayer third fourth 2
            , renderPlayer fifth sixth 3
            , renderPlayer seventh eigth 4
            ]

        _ ->
            [ div [] [ text "Something went wrong." ] ]


renderPlayersContainer : SeedInput -> Html Msg
renderPlayersContainer seedInput =
    case seedInput of
        Empty ->
            div [] []

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
