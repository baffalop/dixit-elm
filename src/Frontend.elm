module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Css
import Helpers exposing (withNoCmd)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode as Decode
import Lamdera exposing (sendToBackend)
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
            ( GameLoading model
            , sendToBackend <| NewPlayerJoined name
            )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg { page, key } =
    let
        noOp =
            page |> withNoCmd
    in
    Tuple.mapFirst (FrontendModel key) <|
        case msg of
            NoOpToFrontend ->
                noOp

            PlayerCouldNotJoin err ->
                Login
                    { name = getName page
                    }
                    |> withNoCmd

            PlayerHasJoined name ->
                case page of
                    InWaitingRoom data ->
                        if data.name == name then
                            noOp

                        else
                            InWaitingRoom { data | players = name :: data.players }
                                |> withNoCmd

                    _ ->
                        InWaitingRoom
                            { name = getName page
                            , cards = []
                            , players = []
                            }
                            |> withNoCmd


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
    [ Html.h1 [] [ Html.text <| "Welcome, " ++ name ]
    , Html.p [] [ Html.text "Other players with you here are:" ]
    , Html.ul [] <| List.map (Html.text >> List.singleton >> Html.li []) players
    ]


getName : Page -> String
getName page =
    case page of
        Login { name } ->
            name

        GameLoading { name } ->
            name

        InWaitingRoom { name } ->
            name
