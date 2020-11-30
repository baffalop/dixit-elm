module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { url = url
      , key = key
      , model =
            LoggingIn
                { name = ""
                }
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ model } as everything) =
    let
        noOp =
            ( model, Cmd.none )
    in
    Tuple.mapFirst (FrontendModel everything.url everything.key) <|
        case msg of
            UrlClicked urlRequest ->
                case urlRequest of
                    Internal url ->
                        ( model
                        , Cmd.batch [ Nav.pushUrl everything.key (Url.toString url) ]
                        )

                    External url ->
                        ( model
                        , Nav.load url
                        )

            UrlChanged url ->
                noOp

            NoOpFrontendMsg ->
                noOp

            LoginSubmitted ->
                case model of
                    LoggingIn data ->
                        ( LoadingLogin data
                        , Cmd.none
                        )

                    _ ->
                        noOp


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view { model } =
    { title = "Dixit"
    , body =
        case model of
            LoggingIn { name } ->
                viewLogin name

            LoadingLogin _ ->
                viewLoading

            InWaitingRoom data ->
                viewWaitingRoom data
    }


viewLogin : String -> List (Html FrontendMsg)
viewLogin name =
    [ H.h1 [] [ H.text "Log In" ]
    , H.form
        [ HE.preventDefaultOn "submit" <| Decode.succeed ( LoginSubmitted, True )
        ]
        [ H.input
            [ HA.placeholder "Choose a name" ]
            [ H.text "Please enter your name" ]
        ]
    ]


viewLoading : List (Html FrontendMsg)
viewLoading =
    [ H.h1 [] [ H.text "Loading..." ]
    ]


viewWaitingRoom : WaitingRoomData -> List (Html FrontendMsg)
viewWaitingRoom { name, cards, players } =
    Debug.todo "waiting room"
