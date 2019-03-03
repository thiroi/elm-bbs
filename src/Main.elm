port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Model =
    { inputTitle : String
    , inputName : String
    , threads : List Thread
    }


type alias Thread =
    { title : String
    , owner : String
    , comments : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" [ Thread "なにはなそう" "ぼぶ" [ "すごいこめんと", "最強のコメント" ], Thread "あたらしいはなし" "まっと" [ "すごいこめんと", "そこそこのコメント" ] ], Cmd.none )



---- UPDATE ----


type Msg
    = Cache
    | UpdateInputTitle String
    | UpdateInputName String
    | MakeThread
    | SyncLocalStorage (List Thread)


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
            ( { model | inputTitle = "", inputName = "" }, portSetLocalStorage ( "threads", Thread model.inputTitle model.inputName [] :: model.threads ) )

        SyncLocalStorage newThreads ->
            ( { model | threads = newThreads }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
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
        , div [] [ text <| "current user id = " ++ model.inputTitle ]
        ]


type alias MyListItem =
    { title : String
    , owner : String
    , numberOfComments : Int
    }


myList : List MyListItem
myList =
    [ { title = "ナニヤルGW", owner = "someone", numberOfComments = 15 }
    , { title = "ナニヤル新年", owner = "anyone", numberOfComments = 32 }
    ]


toTableRow : Thread -> Html Msg
toTableRow threadItem =
    tr []
        [ td [] [ text threadItem.title ]
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }



-- OUTGOING PORT


port portSetLocalStorage : ( String, List Thread ) -> Cmd msg



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions newThreads =
    syncElm SyncLocalStorage



-- INCOMING PORT


port syncElm : (List Thread -> msg) -> Sub msg
