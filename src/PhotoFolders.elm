module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }


type FolderPath
    = End
    | Subfolder Int FolderPath


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [], expanded = False }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson LoadPage modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi", { title = "Trevi", relatedUrls = [ "coli", "fresco" ], size = 34, url = "trevi" } )
                , ( "fresco", { title = "Fresco", relatedUrls = [ "trevi" ], size = 46, url = "fresco" } )
                , ( "coli", { title = "Coliseum", relatedUrls = [ "trevi", "fresco" ], size = 36, url = "coli" } )
                ]
        , root =
            Folder
                { name = "Photos"
                , photoUrls = []
                , expanded = True
                , subfolders =
                    [ Folder
                        { name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , expanded = True
                        , subfolders =
                            [ Folder { name = "outdoors", photoUrls = [], expanded = True, subfolders = [] }
                            , Folder { name = "indoors", photoUrls = [ "fresco" ], expanded = True, subfolders = [] }
                            ]
                        }
                    , Folder
                        { name = "2017"
                        , photoUrls = []
                        , expanded = True
                        , subfolders =
                            [ Folder { name = "outdoors", photoUrls = [], expanded = True, subfolders = [] }
                            , Folder { name = "indoors", photoUrls = [], expanded = True, subfolders = [] }
                            ]
                        }
                    ]
                }
        }


type Msg
    = SelectPhotoUrl String
    | LoadPage (Result Http.Error Model)
    | ToggleExpanded FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPhotoUrl url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        LoadPage (Ok newModel) ->
            ( newModel, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )

        ToggleExpanded path ->
            ( { model | root = toggleExapnded path model.root }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selecedPhoto : Html Msg
        selecedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selecedPhoto ]
        ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubFolder : Int -> Folder -> Html Msg
        viewSubFolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ToggleExpanded path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.indexedMap viewSubFolder folder.subfolders
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewReleatedPhoto photo.relatedUrls)
        ]


toggleExapnded : FolderPath -> Folder -> Folder
toggleExapnded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExapnded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }


viewReleatedPhoto : String -> Html Msg
viewReleatedPhoto url =
    img
        [ class "selected-photos"
        , onClick (SelectPhotoUrl url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
