module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import WebSocket
import Json.Decode exposing (..)
import Dict exposing (..)


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


echoServer : String
echoServer =
    "ws://localhost:8080/myapp"



-- MODEL


type alias Row =
    Dict String Int


type alias Model =
    { rows : List Row
    , alert : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] "", Cmd.none )



-- UPDATE


type Msg
    = NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMessage str ->
            case decodeString (Json.Decode.list (dict int)) str of
                Ok rows ->
                    ( { model | rows = rows }, Cmd.none )

                Err alert ->
                    ( { model | alert = alert }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen echoServer NewMessage



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewMessage model.alert
        , viewTable model.rows
        ]


viewTable : List Row -> Html Msg
viewTable rows =
    case List.head rows of
        Nothing ->
            div [] []

        Just firstrow ->
            table []
                ((viewHeader firstrow) :: (List.map viewRow rows))


viewHeader : Row -> Html Msg
viewHeader firstrow =
    tr [] (List.map viewHeaderCell (Dict.keys firstrow))


viewHeaderCell : String -> Html Msg
viewHeaderCell x =
    th [] [ text x ]


viewRow : Row -> Html Msg
viewRow row =
    tr [] (List.map viewCell (Dict.values row))


viewCell : a -> Html Msg
viewCell x =
    td [] [ text (toString x) ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
