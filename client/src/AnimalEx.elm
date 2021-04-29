module AnimalEx exposing (..)

import Browser
import Html exposing (..)
import Json.Decode
import Html.Events exposing (on)
import Debug exposing (..)
import Html.Events exposing (onInput)


type Msg = 
    AnimalSelected String
    | MyInput String

type alias Model = { animals: List String
                   , selected: String
                   , myinput: String
                   }


selectedAnimalDecoder: Json.Decode.Decoder Msg 
selectedAnimalDecoder = 
    Json.Decode.map AnimalSelected Html.Events.targetValue



initialModel: Model
initialModel =
    {animals= 
        [ "Dog"
        , "Cat"
        , "Hamster"
        ]
    ,selected=""
    ,myinput="Hello"
    }

view : Model-> Html Msg 
view model = 
    div [] [ select [ onChange AnimalSelected ] (List.map animalOptions model.animals)
           , input [onInput MyInput] []
           , input [onMyInput MyInput] []
           , p [] [text model.selected]
           , p [] [text model.myinput]
           ]


onChange : (String -> msg) -> Html.Attribute msg 
onChange tagger = 
    on "change" (Json.Decode.map tagger (log "event" Html.Events.targetValue))

onMyInput : (String -> msg) -> Html.Attribute msg 
onMyInput tagger = 
    on "input" (Json.Decode.map tagger (log "event2" Html.Events.targetValue))

animalOptions : String -> Html a
animalOptions animal = 
    option [] [text animal]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        AnimalSelected animal -> ({model | selected=animal}, Cmd.none)
        MyInput myinput -> ({model| myinput=myinput }, Cmd.none)



main : Program () Model Msg
main = --This returns ( Model, Cmd Msg)
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none}