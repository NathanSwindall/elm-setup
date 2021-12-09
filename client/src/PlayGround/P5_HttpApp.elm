module PlayGround.P5_HttpApp exposing (..)

--import PlayGround.P5_HttpApp exposing (..)

import Array
import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Decode exposing (Decoder)
import Random
import Xml.Decode 
import Xml.Decode exposing (requiredPath)



animeNewsNetwork : String 
animeNewsNetwork = "https://cdn.animenewsnetwork.com/encyclopedia/api.xml?anime=4658"

type Msg = 
     GetAnimeData (Result Http.Error String)
    | DecodeAnimeData String

type alias AnimeData = 
    { info_id : List String 
    , name : String
    , info : List String}

type Status = 
    Loading 
    | Loaded String
    | Errored String

type alias Model = {status: Status
                   , animeData: AnimeData
                   }


animeDecoder = 
    Xml.Decode.succeed AnimeData
        |> requiredPath ["anime", "info"] (Xml.Decode.list (Xml.Decode.stringAttr "gid"))
        |> requiredPath ["anime"] (Xml.Decode.single (Xml.Decode.stringAttr "name"))
        |> requiredPath ["anime", "info"] (Xml.Decode.leakyList Xml.Decode.string) 



view : Model -> Html Msg 
view model = 
    div [ class "content"] <|
        case model.status of
            Loading -> 
                [ h1 [] [ text "Loading right now"]]
            Loaded ad -> 
                [ h1 [] [ text model.animeData.name ]
                , div [] 
                    (List.map (\x -> (h2 [] [text x])) model.animeData.info)
                ]
            Errored errmsg -> 
                [ h1 [] [ text errmsg]
                ]


statusToString status = 
    case status of 
        Loaded data -> data 
        Loading -> ""
        Errored errmsg -> errmsg

decodeAnimeData : String -> Result String AnimeData
decodeAnimeData = Xml.Decode.decodeString animeDecoder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
        case msg of 
            GetAnimeData (Ok data) ->
                update (DecodeAnimeData data) ({model | status = Loaded data})
            GetAnimeData (Err _) -> 
                ( {model | status = Errored "Something went wrong"}, Cmd.none)
            DecodeAnimeData data -> 
                case (decodeAnimeData data) of 
                    (Ok animeData) ->
                         ({model | animeData = animeData}, Cmd.none)
                    (Err _) ->
                        ({model | status = Errored "Decoder failed" }, Cmd.none)


            

initialModel : Model
initialModel = {status = Loading
               ,animeData= initialAnimeData}

initialAnimeData : AnimeData 
initialAnimeData = { info = [], name= "", info_id =[]}


initialCmd: Cmd Msg 
initialCmd = 
    Http.get
        { url = animeNewsNetwork
        , expect = Http.expectString GetAnimeData
        }

main: Program () Model Msg
main = 
    Browser.element
    { init = \flags -> (initialModel, initialCmd)
    , view = view 
    , update = update
    , subscriptions = \model -> Sub.none}
