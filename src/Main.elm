port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Error)
import Url



---- PROGRAM ----


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , inputTitle : String
    , inputName : String
    , threads : List Thread
    }


type alias Thread =
    { id : Int
    , title : String
    , owner : String
    , comments : List Comment
    }


type alias Comment =
    { user : String
    , message : String
    }


flagsDecoder : Decoder (List Thread)
flagsDecoder =
    Decode.list threadDecoder


threadDecoder : Decoder Thread
threadDecoder =
    Decode.map4 Thread
        (Decode.field "id" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.field "owner" Decode.string)
        (Decode.field "comments" (Decode.list commentDecoder))


commentDecoder : Decoder Comment
commentDecoder =
    Decode.map2 Comment
        (Decode.field "user" Decode.string)
        (Decode.field "message" Decode.string)


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            case Decode.decodeValue flagsDecoder flags of
                Err _ ->
                    [ Thread 1 "なにはなそう" "ぼぶ" [ Comment "Sam" "すごいこめんと", Comment "えんどう" "最強のコメント" ], Thread 2 "あたらしいはなし" "まっと" [ Comment "すーちゃん" "すごいこめんと", Comment "名無し" "そこそこのコメント" ] ]

                Ok model ->
                    model
    in
    ( Model key url "" "" initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Cache
    | UpdateInputTitle String
    | UpdateInputName String
    | MakeThread
    | SyncLocalStorage (List Thread)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInputTitle title ->
            ( { model | inputTitle = title }, Cmd.none )

        UpdateInputName name ->
            ( { model | inputName = name }, Cmd.none )

        Cache ->
            ( model, Cmd.none )

        MakeThread ->
            ( { model | inputTitle = "", inputName = "" }, portSetLocalStorage ( "threads", Thread (getNewId model) model.inputTitle model.inputName [] :: model.threads ) )

        SyncLocalStorage newThreads ->
            ( { model | threads = newThreads }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


getNewId : Model -> Int
getNewId model =
    List.length model.threads + 1



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    if String.contains "thread" (Url.toString model.url) then
        { title = "Thread Detail"
        , body =
            [ detailView model
            ]
        }

    else
        { title = "All Thread"
        , body =
            [ allView model
            ]
        }



----- 一覧用


allView : Model -> Html Msg
allView model =
    div [ class "center" ]
        [ div [ class "inputArea" ]
            [ div []
                [ label [] [ text "てーま：" ]
                , input [ value model.inputTitle, onInput UpdateInputTitle ] []
                ]
            , div []
                [ label [] [ text "おなまえ：" ]
                , input [ value model.inputName, onInput UpdateInputName ] []
                ]
            , button [ onClick MakeThread ] [ text "さくせい" ]
            , label [] [ text "" ]
            ]
        , div [ class "threads" ]
            [ tableView model.threads
            ]
        ]


toTableRow : Thread -> Html Msg
toTableRow threadItem =
    tr []
        [ td [] [ viewLink threadItem.title ("/thread/" ++ String.fromInt threadItem.id) ]
        , td [] [ text threadItem.owner ]
        , td [] [ text (String.fromInt (List.length threadItem.comments)) ]
        ]


tableView : List Thread -> Html Msg
tableView threadList =
    table
        [ class "center" ]
        ([ thead []
            [ th [] [ text "てーま" ]
            , th [] [ text "おーなー" ]
            , th [] [ text "こめんと" ]
            ]
         ]
            ++ List.map toTableRow threadList
        )


viewLink : String -> String -> Html msg
viewLink label path =
    div [] [ a [ href path ] [ text label ] ]



----- コメント用


detailView : Model -> Html Msg
detailView model =
    let
        thread =
            findTargetThread model
    in
    div []
        [ div
            []
            [ text thread.title ]
        , commentsView thread.comments
        , div
            []
            [ viewLink "back" "/" ]
        ]


commentsView : List Comment -> Html Msg
commentsView commentList =
    table
        [ class "center" ]
        (List.map toCommentCard commentList)


toCommentCard : Comment -> Html msg
toCommentCard comment =
    div []
        [ h1 [] [ text comment.user ]
        , text comment.message
        ]


findTargetThread : Model -> Thread
findTargetThread model =
    let
        maybeId =
            String.split "/" (Url.toString model.url)
                |> List.reverse
                |> List.head

        targetId =
            case maybeId of
                Nothing ->
                    "999999"

                Just id ->
                    id

        targetModelMaybe =
            model.threads
                |> List.filter (\thread -> String.fromInt thread.id == targetId)
                |> List.head
    in
    case targetModelMaybe of
        Nothing ->
            Thread 99999999 "なにはなそう" (Url.toString model.url) [ Comment "Sam" "すごいこめんと", Comment "えんどう" "最強のコメント" ]

        Just thread ->
            thread



-- OUTGOING PORT


port portSetLocalStorage : ( String, List Thread ) -> Cmd msg



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions newThreads =
    syncElm SyncLocalStorage



-- INCOMING PORT


port syncElm : (List Thread -> msg) -> Sub msg
