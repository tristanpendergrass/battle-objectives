module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, img, input, label, li, ol, p, text)
import Html.Attributes exposing (alt, class, for, href, id, placeholder, src, target, value)
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seedInput = Empty
      , showPlayer1Cards = False
      , showPlayer2Cards = False
      , showPlayer3Cards = False
      , showPlayer4Cards = False
      }
    , Cmd.none
    )


type Msg
    = HandleSeedInput String
    | ToggleShowPlayer1Cards
    | ToggleShowPlayer2Cards
    | ToggleShowPlayer3Cards
    | ToggleShowPlayer4Cards
    | FillRandomSeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSeedInput "" ->
            ( { model
                | seedInput = Empty
                , showPlayer1Cards = False
                , showPlayer2Cards = False
                , showPlayer3Cards = False
                , showPlayer4Cards = False
              }
            , Cmd.none
            )

        HandleSeedInput str ->
            ( { model | seedInput = SeedValue str }, Cmd.none )

        ToggleShowPlayer1Cards ->
            ( { model | showPlayer1Cards = not model.showPlayer1Cards }, Cmd.none )

        ToggleShowPlayer2Cards ->
            ( { model | showPlayer2Cards = not model.showPlayer2Cards }, Cmd.none )

        ToggleShowPlayer3Cards ->
            ( { model | showPlayer3Cards = not model.showPlayer3Cards }, Cmd.none )

        ToggleShowPlayer4Cards ->
            ( { model | showPlayer4Cards = not model.showPlayer4Cards }, Cmd.none )

        FillRandomSeed ->
            let
                randomChar : Random.Generator Char
                randomChar =
                    Random.map (\num -> Char.fromCode num) (Random.int 97 122)

                randomString : Random.Generator String
                randomString =
                    Random.map String.fromList (Random.list 10 randomChar)
            in
            ( model, Random.generate HandleSeedInput randomString )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
        [ class "player"
        , class
            (if showCards then
                "flip"

             else
                ""
            )
        ]
        [ div [ class "player-title" ] [ text ("Player " ++ String.fromInt index) ]
        , button [ class "toggle", onClick handleClick ]
            [ text
                (if showCards then
                    "Hide"

                 else
                    "Show"
                )
            ]
        , div [ class "cards-container" ]
            [ renderCard (imageUrl first)
            , renderCard (imageUrl second)
            ]
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
            div [ class "players-container" ] []

        SeedValue seedValue ->
            div [ class "players-container" ] (renderPlayers model seedValue)


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
        [ div []
            [ label [ for "seed-input" ] [ text "Seed Input" ]
            , input [ id "seed-input", onInput HandleSeedInput, placeholder "Enter seed", value seedInputValue ] [ text "" ]
            ]
        , div [] [ button [ onClick FillRandomSeed ] [ text "Generate Random Seed" ] ]
        ]


renderTopSection : Html Msg
renderTopSection =
    div []
        [ h1 [] [ text "Gloomhaven Battle Goals Generator" ]
        , p [] [ text "How to use:" ]
        , ol []
            [ li [] [ text "Decide with your group on any suitable seed (e.g. 'myseed1234') and which player is which number." ]
            , li [] [ text "Enter the seed in the box below and then click the 'Show' button for your number." ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "top-section" ] [ renderTopSection ]
        , div [ class "main-section" ]
            [ renderSeedInput model
            , renderPlayersContainer model
            ]
        , div [ class "source-link" ]
            [ a [ href "https://github.com/tristanpendergrass/battle-objectives", target "_blank" ]
                [ img [ alt "Github Mark", class "github-mark", src "GitHub-Mark-32px.png" ] []
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
