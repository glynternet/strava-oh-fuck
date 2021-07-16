port module Main exposing (main)

import Base64.Encode as Base64
import Browser exposing (Document, application)
import Browser.Navigation as Navigation exposing (Key)
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Element
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http exposing (Error(..))
import Json.Decode as Json exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (optional)
import Loading exposing (LoaderType(..))
import OAuth
import OAuth.AuthorizationCode as OAuth
import Result.Extra
import Url exposing (Protocol(..), Url)


stravaHost =
    "www.strava.com"


stravaAPIBasePath =
    "api/v3"


ohFuckTitleText =
    "😳 oh fuck"


main : Program Json.Value Model Msg
main =
    application
        { init =
            init
        , update =
            update
        , subscriptions =
            always <| randomBytes GotRandomBytes
        , onUrlRequest =
            always NoOp
        , onUrlChange =
            always NoOp
        , view =
            view { title = ohFuckTitleText }
        }


{-| OAuth configuration.

Note that this demo also fetches basic user information with the obtained access token,
hence the user info endpoint and JSON decoder

-}
configuration : Configuration
configuration =
    { authorizationEndpoint =
        { defaultHttpsUrl | host = stravaHost, path = "/oauth/authorize" }
    , tokenEndpoint =
        { defaultHttpsUrl | host = "localhost", path = "/oauth/token", port_ = Just 9999, protocol = Http }
    , clientId =
        "7102"
    , scope =
        [ "read_all" ]
    }



--
-- Model
--


type alias Model =
    { redirectUri : Url
    , flow : Flow
    , routeIDInput : String
    , segmentSort : SegmentSort
    , routeState : RouteFlow
    }


type SegmentSort
    = Name
    | Distance
    | AverageGrade
    | MaximumGrade


type RouteFlow
    = NoRouteRequested
    | RouteRequested
    | RouteRequestError RouteRequestError
    | Success Route


{-| This demo evolves around the following state-machine\*

        +--------+
        |  Idle  |
        +--------+
             |
             | Redirect user for authorization
             |
             v
     +--------------+
     |  Authorized  |
     +--------------+
             |
             | Exchange authorization code for an access token
             |
             v
    +-----------------+
    |  Authenticated  |
    +-----------------+
             |
             | Fetch user info using the access token
             v
         +--------+
         |  Done  |
         +--------+

(\*) The 'Errored' state hasn't been represented here for simplicity.

-}
type Flow
    = Idle
    | Authorized OAuth.AuthorizationCode
    | Authenticated OAuth.Token
    | Errored Error


type Error
    = ErrInit String
    | ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetUserInfo


type alias Configuration =
    { authorizationEndpoint : Url
    , tokenEndpoint : Url
    , clientId : String
    , scope : List String
    }


{-| During the authentication flow, we'll run twice into the `init` function:

  - The first time, for the application very first run. And we proceed with the `Idle` state,
    waiting for the user (a.k.a you) to request a sign in.

  - The second time, after a sign in has been requested, the user is redirected to the
    authorization server and redirects the user back to our application, with a code
    and other fields as query parameters.

When query params are present (and valid), we consider the user `Authorized`.

-}



--init : Maybe { state : String } -> Url -> Key -> ( Model, Cmd Msg )


init : Json.Value -> Url -> Key -> ( Model, Cmd Msg )
init jsonValue origin navigationKey =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString redirectUri)
    in
    case Json.decodeValue flagsDecoder jsonValue of
        Err err ->
            ( { flow = Errored <| ErrInit <| "Error decoding application state: " ++ Json.errorToString err
              , redirectUri = redirectUri
              , routeState = NoRouteRequested
              , routeIDInput = ""
              , segmentSort = Distance
              }
            , Cmd.none
            )

        Ok flags ->
            let
                baseModel =
                    { flow = Idle
                    , redirectUri = redirectUri
                    , routeState = NoRouteRequested
                    , routeIDInput = Maybe.withDefault "" flags.routeID
                    , segmentSort = Distance
                    }
            in
            case OAuth.parseCode origin of
                OAuth.Empty ->
                    case flags.token of
                        Nothing ->
                            ( baseModel, Cmd.none )

                        Just token ->
                            ( { baseModel
                                | flow =
                                    OAuth.tokenFromString token
                                        |> Maybe.map Authenticated
                                        |> Maybe.withDefault Idle
                              }
                            , Cmd.none
                            )

                -- It is important to set a `state` when making the authorization request
                -- and to verify it after the redirection. The state can be anything but its primary
                -- usage is to prevent cross-site request forgery; at minima, it should be a short,
                -- non-guessable string, generated on the fly.
                --
                -- We remember any previously generated state  state using the browser's local storage
                -- and give it back (if present) to the elm application upon start
                OAuth.Success { code, state } ->
                    if state /= Maybe.map intListToString flags.storedState then
                        ( { baseModel | flow = Errored ErrStateMismatch }, clearUrl )

                    else
                        ( { baseModel | flow = Authorized code }
                        , Cmd.batch
                            [ getAccessToken configuration redirectUri code
                            , clearUrl
                            ]
                        )

                OAuth.Error error ->
                    ( { baseModel | flow = Errored <| ErrAuthorization error }, clearUrl )


type alias Flags =
    { storedState : Maybe (List Int)
    , token : Maybe String
    , routeID : Maybe String
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Json.map3 Flags
        (Json.maybe (Json.field "stored_state" (Json.list Json.int)))
        (Json.maybe (Json.field "token" Json.string))
        (Json.maybe (Json.field "route_id" Json.string))



--
-- Msg
--


type Msg
    = NoOp
    | SignInRequested
    | GotRandomBytes (List Int)
    | GotAccessToken (Result Http.Error OAuth.AuthenticationSuccess)
    | RouteIDTextInputUpdated String
    | GetRoute String
    | GotRoute (Result RouteRequestError Route)
    | SignOutRequested
    | SegmentSortSelected SegmentSort


getAccessToken : Configuration -> Url -> OAuth.AuthorizationCode -> Cmd Msg
getAccessToken { clientId, tokenEndpoint } redirectUri code =
    Http.request <|
        OAuth.makeTokenRequest GotAccessToken
            { credentials =
                { clientId = clientId

                -- secret must be injected by Strava API proxy
                , secret = Nothing
                }
            , code = code
            , url = tokenEndpoint
            , redirectUri = redirectUri
            }



{- On the JavaScript's side, we have:

   app.ports.genRandomBytes.subscribe(n => {
     const buffer = new Uint8Array(n);
     crypto.getRandomValues(buffer);
     const bytes = Array.from(buffer);
     localStorage.setItem("bytes", bytes);
     app.ports.randomBytes.send(bytes);
   });
-}


port genRandomBytes : Int -> Cmd msg


port randomBytes : (List Int -> msg) -> Sub msg


port storeToken : String -> Cmd msg


port deleteToken : () -> Cmd msg


port storeRouteID : String -> Cmd msg



--
-- Update
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.flow, msg ) of
        ( Idle, SignInRequested ) ->
            signInRequested model

        ( Idle, GotRandomBytes bytes ) ->
            gotRandomBytes model bytes

        ( Authorized _, GotAccessToken authenticationResponse ) ->
            gotAccessToken model authenticationResponse

        ( Authenticated _, RouteIDTextInputUpdated id ) ->
            ( { model | routeIDInput = id }, storeRouteID id )

        ( Authenticated token, GetRoute id ) ->
            ( { model | routeState = RouteRequested }
            , getRouteById token id
            )

        ( Authenticated _, GotRoute res ) ->
            ( gotRoute model res, Cmd.none )

        ( Authenticated _, SegmentSortSelected sort ) ->
            ( { model
                | segmentSort = sort
                , routeState =
                    case model.routeState of
                        Success route ->
                            Success { route | segments = Maybe.map (sortSegments sort) route.segments }

                        _ ->
                            model.routeState
              }
            , Cmd.none
            )

        ( _, SignOutRequested ) ->
            signOutRequested model

        other ->
            let
                _ =
                    Debug.log "Received some message but not sure what" other
            in
            noOp model


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


signInRequested : Model -> ( Model, Cmd Msg )
signInRequested model =
    ( { model | flow = Idle }
    , genRandomBytes 16
    )


gotRandomBytes : Model -> List Int -> ( Model, Cmd Msg )
gotRandomBytes model bytes =
    let
        { state } =
            convertBytes bytes

        authorization =
            { clientId = configuration.clientId
            , redirectUri = model.redirectUri
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    ( { model | flow = Idle }
    , authorization
        |> OAuth.makeAuthorizationUrl
        |> Url.toString
        |> Navigation.load
    )


gotAccessToken : Model -> Result Http.Error OAuth.AuthenticationSuccess -> ( Model, Cmd Msg )
gotAccessToken model authenticationResponse =
    case authenticationResponse of
        Err (Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    ( { model | flow = Errored <| ErrAuthentication error }
                    , Cmd.none
                    )

                _ ->
                    ( { model | flow = Errored ErrHTTPGetAccessToken }
                    , Cmd.none
                    )

        Err _ ->
            ( { model | flow = Errored ErrHTTPGetAccessToken }
            , Cmd.none
            )

        Ok { token } ->
            ( { model | flow = Authenticated token }
            , storeToken <| OAuth.tokenToString token
            )


signOutRequested : Model -> ( Model, Cmd Msg )
signOutRequested model =
    ( { model | flow = Idle }
    , Cmd.batch
        [ Navigation.load (Url.toString model.redirectUri)
        , deleteToken ()
        ]
    )


gotRoute : Model -> Result RouteRequestError Route -> Model
gotRoute model res =
    { model
        | routeState =
            res
                |> Result.Extra.unpack
                    RouteRequestError
                    (\route ->
                        Success
                            { route
                                | segments = route.segments |> Maybe.map (sortSegments model.segmentSort)
                            }
                    )
    }


sortSegments : SegmentSort -> List SegmentSummary -> List SegmentSummary
sortSegments sort =
    List.reverse
        << (case sort of
                Distance ->
                    List.sortBy .distance

                Name ->
                    List.sortBy .name

                AverageGrade ->
                    List.sortBy .averageGrade

                MaximumGrade ->
                    List.sortBy .maximumGrade
           )



--
-- View
--


type alias ViewConfiguration =
    { title : String
    }


view : ViewConfiguration -> Model -> Document Msg
view { title } model =
    { title = title
    , body =
        [ Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.paddingXY 10 20
            , Font.family [ Font.typeface "Helvetica" ]
            ]
            (viewBody model)
        ]
    }


viewBody : Model -> Element.Element Msg
viewBody model =
    Element.column [ Element.width Element.fill, Element.height Element.fill ] <|
        case model.flow of
            Idle ->
                [ viewIdle ]

            Authorized _ ->
                [ viewAuthorized ]

            Authenticated _ ->
                [ viewAuthenticated model.routeIDInput model.routeState ]

            Errored err ->
                [ viewError err ]


viewIdle : Element.Element Msg
viewIdle =
    Input.button []
        { onPress = Just SignInRequested
        , label = Element.text "Sign in"
        }


viewAuthorized : Element.Element Msg
viewAuthorized =
    Element.text "Authenticating..."


viewAuthenticated : String -> RouteFlow -> Element.Element Msg
viewAuthenticated inputRouteID routeState =
    Element.row [ Element.width Element.fill, Element.height Element.fill, Element.spacing 10 ]
        [ Element.column
            [ Element.height Element.fill
            , Element.padding 10
            , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
            , Border.color <| color.lightGrey
            ]
            [ Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ Element.el [ Element.centerX, Element.alignTop ] <| Element.text ohFuckTitleText
                , Element.column [ Element.centerY, Element.spacing 10 ]
                    [ Input.text [ Font.size 12, Element.padding 5 ]
                        { onChange = RouteIDTextInputUpdated
                        , text = inputRouteID
                        , placeholder = Just <| Input.placeholder [] <| Element.text "Enter route ID..."
                        , label = Input.labelHidden "Route ID"
                        }
                    , Input.button
                        buttonAttributes
                        { onPress = Just <| GetRoute inputRouteID, label = Element.text "🏃 fetch segments" }
                    , Input.button
                        buttonAttributes
                        { onPress = Just SignOutRequested, label = Element.text "👋 sign out" }
                    ]
                ]
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 10
            , Element.spacing 10
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                ]
                (case routeState of
                    RouteRequestError err ->
                        case err of
                            Unauthorised id ->
                                [ Element.text <| "Error getting the route: unauthorised: do you have permissions to view the with ID " ++ id ++ "?"
                                , Element.paragraph []
                                    [ Element.text "If you continue to experience this, try "
                                    , Input.button []
                                        { onPress = Just SignOutRequested
                                        , label = Element.text "signing out"
                                        }
                                    , Element.text <| " and starting again."
                                    ]
                                ]

                            GeneralErr msg ->
                                [ Element.text <| "Error getting the route: " ++ msg ]

                            NotFound id ->
                                [ Element.text <| "Route " ++ id ++ " not found." ]

                    NoRouteRequested ->
                        []

                    RouteRequested ->
                        [ Element.el
                            [ Element.centerY
                            , Element.centerX
                            ]
                          <|
                            Element.html <|
                                Loading.render
                                    DoubleBounce
                                    loaderConfig
                                    Loading.On
                        ]

                    Success route ->
                        viewRoute route
                )
            ]
        ]


loaderConfig : Loading.Config
loaderConfig =
    { size = 60
    , color = color.orangeCssString
    , className = ""
    , speed = 1
    }


viewRoute : Route -> List (Element.Element Msg)
viewRoute route =
    [ Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.paragraph [ Font.size 30, Font.center, Font.bold ]
            [ case ( route.name, route.idStr ) of
                ( Just name, Just id ) ->
                    Element.newTabLink []
                        { url = routeURL id
                        , label = Element.text name
                        }

                ( Nothing, Just id ) ->
                    Element.newTabLink []
                        { url = routeURL id
                        , label = Element.text <| "Route " ++ id
                        }

                ( Just name, Nothing ) ->
                    Element.text name

                ( Nothing, Nothing ) ->
                    Element.text "Untitled Route"
            ]

        -- workaround for a bug: it's necessary to wrap `table` in an `el`
        -- to get table height attribute to apply
        , Element.el [ Element.width Element.fill, Element.height Element.fill, Element.scrollbars ] <|
            Element.table
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 10
                , Font.size 15
                ]
                { data = Maybe.withDefault [] route.segments
                , columns =
                    [ { header =
                            Input.button [ Element.width <| Element.fillPortion 2, Font.bold ] { onPress = Just <| SegmentSortSelected Name, label = Element.text "Segment" }
                      , width = Element.fillPortion 2
                      , view =
                            \rec ->
                                Element.newTabLink
                                    [ Element.width Element.fill
                                    , Font.alignLeft
                                    ]
                                    { url = segmentURL rec.id, label = Element.text rec.name }
                      }
                    , { header =
                            Input.button [ Element.width <| Element.fillPortion 1, Font.bold, Font.center ] { onPress = Just <| SegmentSortSelected Distance, label = Element.text "Distance (m)" }
                      , width = Element.fillPortion 1
                      , view =
                            \rec ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Font.center
                                    ]
                                    (Element.text <| String.fromFloat rec.distance)
                      }
                    , { header =
                            Input.button [ Element.width <| Element.fillPortion 1, Font.bold, Font.center ] { onPress = Just <| SegmentSortSelected AverageGrade, label = Element.text "Average Grade (%)" }
                      , width = Element.fillPortion 1
                      , view =
                            \rec ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Font.center
                                    ]
                                    (Element.text <| String.fromFloat rec.averageGrade)
                      }
                    , { header =
                            Input.button [ Element.width <| Element.fillPortion 1, Font.bold, Font.center ] { onPress = Just <| SegmentSortSelected MaximumGrade, label = Element.text "Maximum Grade (%)" }
                      , width = Element.fillPortion 1
                      , view =
                            \rec ->
                                Element.el
                                    [ Element.width Element.fill
                                    , Font.center
                                    ]
                                    (Element.text <| String.fromFloat rec.maximumGrade)
                      }
                    ]
                }
        ]
    ]


viewError : Error -> Element.Element Msg
viewError e =
    Element.text <|
        case e of
            ErrStateMismatch ->
                "'state' doesn't match, the request has likely been forged by an adversary!"

            ErrAuthorization error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrAuthentication error ->
                oauthErrorToString { error = error.error, errorDescription = error.errorDescription }

            ErrHTTPGetAccessToken ->
                "Unable to retrieve token: HTTP request failed. CORS is likely disabled on the authorization server."

            ErrHTTPGetUserInfo ->
                "Unable to retrieve user info: HTTP request failed."

            ErrInit msg ->
                msg


buttonAttributes : List (Element.Attribute Msg)
buttonAttributes =
    [ Element.width Element.fill
    , Font.size 12
    , Font.color color.white
    , Element.padding 10
    , Element.spacingXY 10 10
    , Border.rounded 6
    , Element.Background.color color.orange
    ]


color =
    { black = Element.rgb255 0 0 0
    , blue = Element.rgb255 0x72 0x9F 0xCF
    , darkCharcoal = Element.rgb255 0x2E 0x34 0x36
    , lightBlue = Element.rgb255 0xC5 0xE8 0xF7
    , lightGrey = Element.rgb255 0xE0 0xE0 0xE0
    , white = Element.rgb255 0xFF 0xFF 0xFF
    , orange = Element.rgb255 0xFC 0x52 0x00
    , orangeCssString = "#fc5200"
    }



--
-- Helpers
--


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


intListToString : List Int -> String
intListToString =
    toBytes >> base64


convertBytes : List Int -> { state : String }
convertBytes =
    intListToString >> (\state -> { state = state })


oauthErrorToString : { error : OAuth.ErrorCode, errorDescription : Maybe String } -> String
oauthErrorToString { error, errorDescription } =
    let
        desc =
            errorDescription |> Maybe.withDefault "" |> String.replace "+" " "
    in
    OAuth.errorCodeToString error ++ ": " ++ desc


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }



-- Strava


getRouteById : OAuth.Token -> String -> Cmd Msg
getRouteById token id =
    { url = "https://" ++ stravaHost ++ "/" ++ stravaAPIBasePath ++ "/routes/" ++ id
    , expect = Http.expectJson (routeResultMsg id) routeDecoder
    , headers = OAuth.useToken token []
    , method = "GET"
    , body = Http.emptyBody
    , timeout = Nothing
    , tracker = Nothing
    }
        |> Http.request


type RouteRequestError
    = GeneralErr String
    | Unauthorised String
    | NotFound String


routeResultMsg : String -> Result Http.Error Route -> Msg
routeResultMsg id =
    Result.mapError
        (\err ->
            case err of
                BadUrl url ->
                    GeneralErr <| "Bad URL: " ++ url

                Timeout ->
                    GeneralErr <| "Request timed out"

                NetworkError ->
                    GeneralErr <| "Network error"

                BadStatus status ->
                    case status of
                        401 ->
                            Unauthorised id

                        404 ->
                            NotFound id

                        _ ->
                            GeneralErr <| "Unexpected status: " ++ String.fromInt status

                BadBody errMsg ->
                    GeneralErr <| "Bad response body: " ++ errMsg
        )
        >> GotRoute


type alias Route =
    { description : Maybe String
    , distance : Maybe Float
    , elevationGain : Maybe Float

    -- Use idStr because using id will overflow the javascript/elm Int value
    , idStr : Maybe String
    , name : Maybe String
    , private : Maybe Bool
    , starred : Maybe Bool
    , timestamp : Maybe Int
    , type_ : Maybe Int
    , subType : Maybe Int
    , estimatedMovingTime : Maybe Int
    , segments : Maybe (List SegmentSummary)
    }


routeDecoder : Decoder Route
routeDecoder =
    Json.succeed Route
        |> optional "description" (Json.nullable Json.string) Nothing
        |> optional "distance" (Json.nullable Json.float) Nothing
        |> optional "elevation_gain" (Json.nullable Json.float) Nothing
        |> optional "id_str" (Json.nullable Json.string) Nothing
        |> optional "name" (Json.nullable Json.string) Nothing
        |> optional "private" (Json.nullable Json.bool) Nothing
        |> optional "starred" (Json.nullable Json.bool) Nothing
        |> optional "timestamp" (Json.nullable Json.int) Nothing
        |> optional "type" (Json.nullable Json.int) Nothing
        |> optional "sub_type" (Json.nullable Json.int) Nothing
        |> optional "estimated_moving_time" (Json.nullable Json.int) Nothing
        |> optional "segments" (Json.nullable (Json.list segmentSummaryDecoder)) Nothing


type alias SegmentSummary =
    { name : String
    , id : Int
    , distance : Float
    , averageGrade : Float
    , maximumGrade : Float
    }


segmentSummaryDecoder : Decoder SegmentSummary
segmentSummaryDecoder =
    Json.succeed SegmentSummary
        |> Pipeline.required "name" Json.string
        |> Pipeline.required "id" Json.int
        |> Pipeline.required "distance" Json.float
        |> Pipeline.required "average_grade" Json.float
        |> Pipeline.required "maximum_grade" Json.float


segmentURL : Int -> String
segmentURL id =
    "https://" ++ stravaHost ++ "/segments/" ++ String.fromInt id


routeURL : String -> String
routeURL id =
    "https://" ++ stravaHost ++ "/routes/" ++ id
