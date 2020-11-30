module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Helpers exposing (withNoCmd)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
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
    ( { key = key
      , page =
            Login
                { name = ""
                }
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg ({ page } as model) =
    let
        noOp =
            ( page, Cmd.none )
    in
    Tuple.mapFirst (FrontendModel model.key) <|
        case msg of
            UrlClicked urlRequest ->
                case urlRequest of
                    Internal url ->
                        ( page
                        , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                        )

                    External url ->
                        ( page
                        , Nav.load url
                        )

            UrlChanged url ->
                noOp

            NoOpFrontendMsg ->
                noOp

            LoginMsg loginMsg ->
                case page of
                    Login data ->
                        updateLogin loginMsg data

                    _ ->
                        noOp


updateLogin : LoginMsg -> { name : String } -> ( Page, Cmd FrontendMsg )
updateLogin msg ({ name } as model) =
    case msg of
        LoginNameChanged newName ->
            Login { name = newName }
                |> withNoCmd

        LoginSubmitted ->
            GameLoading model
                |> withNoCmd


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view { page } =
    let
        content =
            case page of
                Login data ->
                    viewLogin data

                GameLoading data ->
                    viewLoading data

                InWaitingRoom data ->
                    viewWaitingRoom data
    in
    { title = "Dixit"
    , body =
        content
            |> Html.div
                [ HA.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    , Css.width <| Css.pct 100
                    , Css.margin <| Css.px 30
                    ]
                ]
            |> Html.toUnstyled
            |> List.singleton
    }


viewLogin : { name : String } -> List (Html FrontendMsg)
viewLogin { name } =
    [ Html.h1 [] [ Html.text "Log In" ]
    , Html.form
        [ HE.preventDefaultOn "submit" <| Decode.succeed ( LoginSubmitted, True )
        ]
        [ Html.input
            [ HA.placeholder "Choose a name"
            , HA.required True
            , HE.onInput LoginNameChanged
            ]
            []
        ]
    ]
        |> List.map (Html.map LoginMsg)


viewLoading : { name : String } -> List (Html FrontendMsg)
viewLoading { name } =
    [ Html.h1 [] [ Html.text "Loading..." ]
    , Html.p [] [ Html.text name ]
    ]


viewWaitingRoom : WaitingRoomData -> List (Html FrontendMsg)
viewWaitingRoom { name, cards, players } =
    Debug.todo "waiting room"
