module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, hr, img, input, label, li, ol, p, span, text)
import Html.Attributes exposing (class, for, id, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
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
    , showPlayer1Cards : Bool
    , showPlayer2Cards : Bool
    , showPlayer3Cards : Bool
    , showPlayer4Cards : Bool
    }


initialModel : Model
initialModel =
    { seedInput = Empty
    , showPlayer1Cards = False
    , showPlayer2Cards = False
    , showPlayer3Cards = False
    , showPlayer4Cards = False
    }


type Msg
    = HandleSeedInput String
    | ToggleShowPlayer1Cards
    | ToggleShowPlayer2Cards
    | ToggleShowPlayer3Cards
    | ToggleShowPlayer4Cards


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleSeedInput "" ->
            { model | seedInput = Empty }

        HandleSeedInput str ->
            { model | seedInput = SeedValue str }

        ToggleShowPlayer1Cards ->
            { model | showPlayer1Cards = not model.showPlayer1Cards }

        ToggleShowPlayer2Cards ->
            { model | showPlayer2Cards = not model.showPlayer2Cards }

        ToggleShowPlayer3Cards ->
            { model | showPlayer3Cards = not model.showPlayer3Cards }

        ToggleShowPlayer4Cards ->
            { model | showPlayer4Cards = not model.showPlayer4Cards }


renderCard : String -> Html Msg
renderCard imageUrl =
    div [ class "flip-card" ]
        [ div [ class "flip-card-inner" ]
            [ div [ class "flip-card-back" ] [ img [ class "card", src imageUrl ] [] ]
            , div [ class "flip-card-front" ] [ img [ class "card", src "battle-goal-back.jpg" ] [] ]
            ]
        ]


renderPlayer : Msg -> Bool -> Int -> Int -> Int -> Html Msg
renderPlayer handleClick showCards first second index =
    let
        imageUrl : Int -> String
        imageUrl number =
            String.fromInt number ++ ".jpg"
    in
    div
        [ class
            (if showCards then
                "flip"

             else
                ""
            )
        ]
        [ div
            []
            [ span [] [ text ("Player " ++ String.fromInt index) ]
            , button [ onClick handleClick ] [ text "Toggle" ]
            ]
        , renderCard (imageUrl first)
        , renderCard (imageUrl second)
        ]


renderPlayers : Model -> String -> List (Html Msg)
renderPlayers model seedValue =
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
            [ renderPlayer ToggleShowPlayer1Cards model.showPlayer1Cards first second 1
            , renderPlayer ToggleShowPlayer2Cards model.showPlayer2Cards third fourth 2
            , renderPlayer ToggleShowPlayer3Cards model.showPlayer3Cards fifth sixth 3
            , renderPlayer ToggleShowPlayer4Cards model.showPlayer4Cards seventh eigth 4
            ]

        _ ->
            [ div [] [ text "Something went wrong." ] ]


renderPlayersContainer : Model -> Html Msg
renderPlayersContainer model =
    case model.seedInput of
        Empty ->
            div [] []

        SeedValue seedValue ->
            div [] (renderPlayers model seedValue)


renderSeedInput : Model -> Html Msg
renderSeedInput model =
    let
        seedInputValue : String
        seedInputValue =
            case model.seedInput of
                Empty ->
                    ""

                SeedValue seedValue ->
                    seedValue
    in
    div [ class "seed-input" ]
        [ label [ for "seed-input" ] [ text "Seed Input" ]
        , input [ id "seed-input", onInput HandleSeedInput, placeholder "Enter seed", value seedInputValue ] [ text "" ]
        ]


renderTopSection : Html Msg
renderTopSection =
    div []
        [ h1 [] [ text "Gloomhaven Battle Goals Generator" ]
        , p [] [ text "Steps for use:" ]
        , ol []
            [ li [] [ text "Decide with your group on any suitable seed (e.g. 'foobarbaz') and which player is which number." ]
            , li [] [ text "Enter the seed in the box below and then click the 'Show' button for your number." ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ renderTopSection
        , hr [] []
        , renderSeedInput model
        , renderPlayersContainer model
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
