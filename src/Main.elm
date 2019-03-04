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
    , successMsg : String
    , errMsg : String
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
    , date : String
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
    Decode.map3 Comment
        (Decode.field "user" Decode.string)
        (Decode.field "message" Decode.string)
        (Decode.field "date" Decode.string)


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialThread =
            case Decode.decodeValue flagsDecoder flags of
                Err _ ->
                    [ Thread 1
                        "とあるウワサ"
                        "ぼぶ"
                        [ Comment "KK" "最近、じーのさんがいなくなったことで、Rust会が下火らしいですよ。。。困りましたね。これはどうにかしないと。どうしますか、もっつさん" "2018/09/02 15:13"
                        , Comment "えんどう" "Rust会にかわる、Haskell会でも開いてみては..." "2018/09/02 15:24"
                        , Comment "MT" "Elmヤバイ。シヌ" "2018/09/02 15:24"
                        , Comment "名無し" "とある噂によると、例のあの人が、Uに入るらしいです。大型強化ってやつですね！！！これからどういう開発体制になるのやら" "2018/09/02 15:24"
                        ]
                    , Thread 2 "あたらしいはなし" "まっと" [ Comment "すーちゃん" "すごいこめんと" "2018/09/02 15:24", Comment "名無し" "そこそこのコメント" "2018/09/02 15:32" ]
                    ]

                Ok model ->
                    model
    in
    ( Model key url "" "" "" "" initialThread, Cmd.none )



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
            case validInput model of
                Err errMsg ->
                    ( { model | errMsg = errMsg, successMsg = "" }, Cmd.none )

                Ok successMsg ->
                    ( { model | errMsg = "", successMsg = successMsg, inputTitle = "", inputName = "" }, portSetLocalStorage ( "threads", Thread (getNewId model) model.inputTitle model.inputName [] :: model.threads ) )

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


validInput : Model -> Result String String
validInput model =
    if String.length model.inputTitle == 0 then
        Err "タイトルは必ずいれてね"

    else if String.length model.inputTitle > 256 then
        Err "タイトルが256文字を超えているよ！"

    else if String.length model.inputName == 0 then
        Err "おなまえは必須だよ！"

    else if String.length model.inputName > 32 then
        Err "おなまえは32文字以内にしてみようか"

    else
        Ok ("新しいスレッド「" ++ model.inputTitle ++ "」 をつくったよ！")


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
        [ h1 [] [ text "ELM - BBS" ]
        , div [ class "inputArea" ]
            [ div []
                [ label [] [ text "タイトル：" ]
                , input [ value model.inputTitle, onInput UpdateInputTitle ] []
                ]
            , div []
                [ label [] [ text "おなまえ：" ]
                , input [ value model.inputName, onInput UpdateInputName ] []
                ]
            ]
        , button [ onClick MakeThread ] [ text "さくせい" ]
        , div [ class "msgArea" ]
            [ label [ class "errMsg" ] [ text model.errMsg ]
            , label [ class "successMsg" ] [ text model.successMsg ]
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



----- スレッド詳細画面


detailView : Model -> Html Msg
detailView model =
    let
        thread =
            findTargetThread model
    in
    div []
        [ h1
            [ class "center" ]
            [ text thread.title ]
        , commentsView thread.comments
        , div
            [ class "back" ]
            [ viewLink "いちらんにもどる" "/" ]
        ]


commentsView : List Comment -> Html Msg
commentsView commentList =
    table
        [ class "center" ]
        (List.map toCommentCard commentList)


toCommentCard : Comment -> Html msg
toCommentCard comment =
    div [ class "comment_card" ]
        [ div [ class "comment_date" ] [ text comment.date ]
        , div [ class "comment_user" ] [ text comment.user ]
        , div [ class "comment_message" ] [ text comment.message ]
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
            Thread 99999999 "なにはなそう" (Url.toString model.url) [ Comment "Sam" "すごいこめんと" "2019/01/01 11:09", Comment "えんどう" "最強のコメント" "2019/01/01 11:10" ]

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
