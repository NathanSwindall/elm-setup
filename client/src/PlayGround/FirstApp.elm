module PlayGround.FirstApp exposing (main, view, initialModel)

--for css styling -- might be interesting
-- import Html
-- import Css exposing (..)
-- import Html.Styled exposing (..)
-- import Html.Styled.Attributes exposing (..)
-- import Html.Styled.Events exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random


urlPrefix : String
urlPrefix = 
    "http://elm-in-action.com/"

urlPrefix_local : String
urlPrefix_local=
    "http://localhost:3002/backend_serve/"


type Msg = 
    ClickedPhoto String
    | ClickedSurpriseMe 
    | ClickedSize ThumbnailSize
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)

type ThumbnailSize
    = Small
    | Medium
    | Large

type alias Photo = {url: String}
-- type alias Model =
--     { photos: List Photo
--     , selectedUrl : String
--     , chosensize: ThumbnailSize
--     }

type alias Model = 
    { status : Status
    , chosenSize : ThumbnailSize
    }


type Status 
    = Loading 
    | Loaded (List Photo) String 
    | Errored String


view : Model -> Html Msg
view model = 
    div [ class "content"] <|
        case model.status of
            Loaded photos selectedUrl -> 
                viewLoaded photos selectedUrl model.chosenSize
            Loading -> 
                [ div [] [
                        div [ id "loader-wrapper"]
                        [ div [ id "loader"] []
                        , div [ class "loader-section section-left"] [ text "loading"]
                        , div [ class "loader-section section-right"] [ text "loading"]
                        ]
                    ]
                ]
            Errored errorMessage -> 
                [text ("Error: " ++ errorMessage)]
        

viewLoaded: List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize = 
    [ h1 [] [text "Photo Groove"]
        , button 
            [ onClick ClickedSurpriseMe] 
            [ text "Surpirse Me"]
        , h3 [] [text "Thumbnail Size:"]
        , div [ id "choose-size"]
            (List.map viewSizeChooser [Small, Medium, Large])
        , div [id "thumbnails" ,class (sizeToClass chosenSize)] 
            (List.map (viewThumbnail selectedUrl) photos)
        , img 
            [ class "large"
            , src (urlPrefix_local ++ "large/" ++ selectedUrl)
            ]
            []
        ]

viewThumbnail selectedUrl thumb = 
    img 
        [ src (urlPrefix_local ++ thumb.url)
        , classList [ ("selected", selectedUrl == thumb.url)]
        , onClick (ClickedPhoto thumb.url)
        ]
        []

viewSizeChooser: ThumbnailSize -> Html Msg 
viewSizeChooser size = 
    label []
        [ input [type_ "radio", name "size", onClick (ClickedSize size)] []
        , text (sizeToString size)]

--For the text on the UI
sizeToString : ThumbnailSize -> String 
sizeToString size = 
    case size of 
        Small  -> "small"
        Medium -> "medium"
        Large  -> "large"

--For the classes of the picture
sizeToClass : ThumbnailSize -> String 
sizeToClass size = 
    case size of 
        Small -> "small"
        Medium -> "med"
        Large -> "large"



initialModel : Model 
initialModel = 
    { status = Loading 
    , chosenSize = Medium
    }

initialUrl : String 
initialUrl = "http://localhost:3002/data/json3"

initialCmd : Cmd Msg 
initialCmd = 
    Http.get 
        { url = initialUrl
        , expect = Http.expectString GotPhotos
        }


selectUrl : String -> Status -> Status 
selectUrl url status = 
    case status of 
        Loaded photos _ -> 
            Loaded photos url 
        Loading -> 
            status 
        Errored errorMessage -> 
            status


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        ClickedPhoto url -> 
            ({ model |status = selectUrl url model.status }, Cmd.none)
        ClickedSurpriseMe -> 
            case model.status of 
                Loaded [] _ -> 
                    (model, Cmd.none)
                Loaded (firstPhoto :: otherPhotos) _ -> 
                    Random.uniform firstPhoto otherPhotos
                    |> Random.generate GotRandomPhoto 
                    |> Tuple.pair model --a random command will be there
                Loading -> 
                    (model, Cmd.none)
                Errored errorMessage -> 
                    (model, Cmd.none)
        ClickedSize size -> 
            ({ model | chosenSize = size}, Cmd.none)
        GotRandomPhoto photo-> 
            ({model | status = selectUrl photo.url model.status}, Cmd.none)
        GotPhotos (Ok responseStr) -> 
             case String.split "," responseStr of 
                        (firstUrl :: _) as urls -> 
                            let
                                photos = 
                                    List.map Photo urls 
                            in
                            ( {model | status = Loaded photos firstUrl }, Cmd.none)
                        [] -> 
                            ( { model | status = Errored "0 photos found"}, Cmd.none)
        GotPhotos (Err _) -> 
             ({model | status = Errored "Server error!"}, Cmd.none)

main: Program () Model Msg
main =
    Browser.element
    { init = \_ -> (initialModel, initialCmd)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }