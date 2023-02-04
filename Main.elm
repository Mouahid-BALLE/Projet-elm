module Main exposing (main)

import Browser
import Html exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, list, string)


-- MODEL

type alias Model =
      { word : Word
      , jsonState : State
      , errorMessage : Maybe String
      , typedGuess : Maybe String
      , submittedGuess : Maybe String
      , wordToGuess : String
      , totalGuesses : Int
    }

-- Object pour decoder le Json défintion
type alias Word = List Description

type alias Description =
    { word : String
    , meanings : List Meaning
    }

type alias Meaning =
    { partOfSpeech : String
    , definitions : List Definition
    }

type alias Definition =
    {
      definition : String
    }


type State = Success | Failure | Loading
    
-- VIEW
view : Model -> Html Msg
view model =
 case model.jsonState of
            Success -> div [] (displayDesc model.word ++ [viewInterface model])
      
            Loading -> div [] 
                [ button [ onClick SendHttpRequestWord]
                [ text "Play"]]

            Failure -> div [] 
                [ button [ onClick SendHttpRequestWord]
                    [ text "Play Again"], viewWordOrError model]

viewInterface : Model -> Html Msg
viewInterface model =
    let
       inputDiv = div [] 
          [ div[]
            [ input [ type_ "text", onInput TypedText, model.typedGuess |> Maybe.withDefault "" |> value][]
              , button[ onClick SubmitGuess ][ text "Guess!" ]
              , button[ onClick GiveUp ][ text "Give up!" ]
            ]
            , div [] [ text <| "Guesses: " ++ String.fromInt model.totalGuesses]  
          ]
    in
    case model.submittedGuess of
        Just "I GIVE UP BECAUSE I AM A LOSER" ->
            div [] [  button[ onClick SendHttpRequestWord ][ text "Play Again!" ], div [] [ text <| "The word was..." ++ model.wordToGuess]]
            
        Just guess ->
            if guess == model.wordToGuess then
                div [] [ button[ onClick SendHttpRequestWord ][ text "Play Again!" ], div [] [ text <| "You correctly guessed " ++ model.wordToGuess ] ]

            else
                div [] [ inputDiv, div [] [ text "Incorect try again..." ] ]

        Nothing ->
            div [] [ inputDiv]
            
        
-- Permet d'afficher les défintions obtenus par les décodeurs Json
displayDesc : Word -> List (Html Msg)
displayDesc list = case list of
    [] -> []
    (x::xs) -> [div [] ([ul [] (displayMeanings x.meanings)])] ++ (displayDesc xs) 

displayMeanings : List Meaning -> List (Html Msg)
displayMeanings list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.partOfSpeech]] ++ [ol [] (displayDefinitions x.definitions)] ++ (displayMeanings xs)

displayDefinitions : List Definition -> List (Html Msg)
displayDefinitions list = case list of
    [] -> []
    (x::xs) -> [li [] [text x.definition]] ++ (displayDefinitions xs)
    
viewWordOrError : Model -> Html Msg
viewWordOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
             viewError "message"


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch word at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


-- UPDATE

type Msg
    =  TypedText String
    | SubmitGuess
    | GiveUp
    | SendHttpRequestWord
    | DataReceivedWord (Result Http.Error String)
    | DataReceivedDef (Result Http.Error Word)

-- Requête pour obtenir un mot aléatoire
getWord : Cmd Msg
getWord =
    Http.get
        { url = "https://random-word-api.herokuapp.com/word"
        , expect = Http.expectString DataReceivedWord
        }
-- Requête pour obtenir la défintion du mot aléatoire       
getDef : String -> Cmd Msg
getDef (word)=
    Http.get
        { url = "https://api.dictionaryapi.dev/api/v2/entries/en/"++word
        , expect = Http.expectJson DataReceivedDef wordDecoder
        }
        
-- Décodeurs Json
wordDecoder : Decoder (List Description)
wordDecoder = Decode.list descriptionDecoder

descriptionDecoder : Decoder Description
descriptionDecoder = 
            Decode.map2 Description
              (field "word" string)
              (field "meanings" <| Decode.list meaningDecoder)

meaningDecoder : Decoder Meaning
meaningDecoder =
            Decode.map2 Meaning
              (field "partOfSpeech" string)
              (field "definitions" <| Decode.list definitionDecoder)

definitionDecoder : Decoder Definition
definitionDecoder =
            Decode.map Definition
              (field "definition" string)

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
            
-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypedText inputString ->
            ( { model | typedGuess = Just inputString }
            , Cmd.none
            )

        SubmitGuess ->
            ( { model
                | typedGuess = Nothing
                , submittedGuess = model.typedGuess
                , totalGuesses = model.totalGuesses + 1
              }
            , Cmd.none
            )
            
        GiveUp ->
            ( { model
                | typedGuess = Nothing
                , submittedGuess = Just "I GIVE UP BECAUSE I AM A LOSER"
                , totalGuesses = model.totalGuesses + 1
              }
            , Cmd.none
            )

        SendHttpRequestWord ->
            ( { model | totalGuesses = 0, submittedGuess = model.typedGuess}, getWord )

        DataReceivedWord (Ok wordStr) ->
          let 
            word = String.dropRight 2 (String.dropLeft 2 wordStr)
          in 
            ( {model | wordToGuess = word}, getDef (word) )

        DataReceivedWord (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , getWord
            )
        DataReceivedDef (Ok desc) ->
            ( { model | jsonState = Success , word = desc }, Cmd.none )

        DataReceivedDef (Err httpError) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError), jsonState = Failure
              }
            , getWord
            )
            
-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = []
        , errorMessage = Nothing
        , jsonState = Loading
        , typedGuess = Nothing
        , submittedGuess = Nothing
        , wordToGuess = ""
        , totalGuesses = 0
      }
    , getWord
    )

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }