module Main exposing (Entry, Model, Msg(..), init, initialModel, main, onDragEnd, onDragOver, onDragStart, onDrop, update, view)

import Browser
import Html exposing (Attribute, Html, div, h1, h2, p, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as JD



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { list : List Entry
    , tempList : List Entry
    , draggingEntryId : Int
    , isDragging : Bool
    }


type alias Entry =
    { id : Int
    , name : String
    , content : String
    , dragging : Bool
    , order : Int
    }


type Direction
    = UP
    | DOWN


initialModel : Model
initialModel =
    { list = []
    , tempList = []
    , isDragging = False
    , draggingEntryId = 0
    }


init : List Entry -> ( Model, Cmd Msg )
init list =
    ( { initialModel | list = list }, Cmd.none )



-- UDPATE


type Msg
    = StartDragging Entry
    | EndDragging
    | ReorderEntriesOverDragging Entry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartDragging entry ->
            ( { model
                | isDragging = True
                , draggingEntryId = entry.id
                , tempList = model.list
              }
            , Cmd.none
            )

        EndDragging ->
            ( { model
                | isDragging = initialModel.isDragging
                , list = model.tempList
                , tempList = initialModel.tempList
                , draggingEntryId = initialModel.draggingEntryId
              }
            , Cmd.none
            )

        ReorderEntriesOverDragging entry ->
            let
                currentEntry =
                    model.list |> List.filter (\x -> x.id == model.draggingEntryId)

                direction =
                    case List.head currentEntry of
                        Just x ->
                            if x.order < entry.order then
                                DOWN

                            else
                                UP

                        Nothing ->
                            DOWN

                ( beforeList, afterList ) =
                    model.list
                        |> List.filter (\x -> x.id /= model.draggingEntryId)
                        |> List.partition
                            (if direction == DOWN then
                                \x -> x.order <= entry.order

                             else
                                \x -> x.order < entry.order
                            )

                orderedList =
                    [ beforeList, currentEntry, afterList ]
                        |> List.concat
                        |> List.indexedMap Tuple.pair
                        |> List.map (\( index, x ) -> { x | order = index + 1 })
            in
            ( { model | tempList = orderedList }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        list_ =
            if model.isDragging then
                model.tempList

            else
                model.list
                    |> List.sortBy .order

        isDraggingClass cond =
            if cond then
                " is-dragging"

            else
                ""
    in
    div
        [ Attributes.class
            ("dnd" ++ isDraggingClass (model.isDragging == True))
        ]
        [ h1 [] [ text "dnd-list" ]
        , div [ Attributes.class "dnd__list" ]
            (List.map
                (\entry ->
                    div
                        [ Attributes.class
                            ("dnd__item" ++ isDraggingClass (entry.id == model.draggingEntryId))
                        , Attributes.draggable "true"
                        , onDragStart (StartDragging entry)
                        , onDragEnd EndDragging
                        , onDragOver (ReorderEntriesOverDragging entry)
                        ]
                        [ h2 []
                            [ span [] [ text (String.fromInt entry.id) ]
                            , text entry.name
                            ]
                        , p [] [ text entry.content ]
                        ]
                )
                list_
            )
        ]



-- Event Listeners


onDragStart : msg -> Attribute msg
onDragStart msg =
    Events.on "dragstart" (JD.succeed msg)


onDragEnd : msg -> Attribute msg
onDragEnd msg =
    Events.on "dragend" (JD.succeed msg)


onDragOver : msg -> Attribute msg
onDragOver msg =
    Events.on "dragover" (JD.succeed msg)


onDrop : msg -> Attribute msg
onDrop msg =
    Events.on "drop" (JD.succeed msg)
