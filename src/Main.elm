import Browser
import Html exposing (Html, button, div, text, h1, h2, span, p, Attribute)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
-- import Debug exposing (..)

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

initialModel : Model
initialModel =
  { list = []
  , tempList = []
  , isDragging = False
  , draggingEntryId = 0
  }

init : ( List Entry ) -> ( Model, Cmd Msg )
init list = ( { initialModel | list = list }, Cmd.none)

-- UDPATE

type Msg
  = OnDragStartMsg Entry
  | OnDragEndMsg
  | OnDragOverMsg Entry

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnDragStartMsg entry ->
      ( { model
        | isDragging = True
        , draggingEntryId = entry.id
        , tempList = model.list
        }, Cmd.none )

    OnDragEndMsg ->
      ( { model
        | isDragging = initialModel.isDragging
        , list = model.tempList
        , tempList = initialModel.tempList
        , draggingEntryId = initialModel.draggingEntryId
        }, Cmd.none )

    OnDragOverMsg entry ->
      let
        currentEntry = model.list |> List.filter (\x -> x.id == model.draggingEntryId)
        direction = 
          case List.head currentEntry of
            Just x -> if x.order < entry.order then "DOWN" else "UP"
            Nothing -> "DOWN"
        ( beforeList, afterList ) =
          model.list
            |> List.filter (\x -> x.id /= model.draggingEntryId)
            |> List.partition (if direction == "DOWN" then \x -> (x.order <= entry.order) else \x -> (x.order < entry.order))
        orderedList = 
          [beforeList, currentEntry,  afterList]
            |> List.concat
            |> List.indexedMap Tuple.pair
            |> List.map (\(index, x) -> { x | order = index + 1 })
      in
        ( { model | tempList = orderedList }, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    list_ = 
      if model.isDragging then model.tempList else model.list
        |> List.sortBy .order
  in
    div [ class ("dnd" ++ if model.isDragging == True then " is-dragging" else "") ] [
      h1 [] [ text "dnd-list" ],
      div [ class "dnd__list" ] (List.map (\entry -> (
        div [ class ("dnd__item" ++ (if entry.id == model.draggingEntryId then " dnd__item--is-dragging" else "" ))
        , draggable "true" 
        , onDragStart (OnDragStartMsg entry)
        , onDragEnd OnDragEndMsg
        , onDragOver (OnDragOverMsg entry)
        ] [
            h2 [] [
              span [] [ text (String.fromInt entry.id) ],
              text entry.name
            ],
            p [] [ text entry.content ]
          ]
      )) list_)
    ]


-- Event Listeners

onDragStart : msg -> Attribute msg
onDragStart msg =
    on "dragstart" (Json.succeed msg)

onDragEnd : msg -> Attribute msg
onDragEnd msg =
    on "dragend" (Json.succeed msg)

onDragOver : msg -> Attribute msg
onDragOver msg =
    on "dragover" (Json.succeed msg)

onDrop : msg -> Attribute msg
onDrop msg =
    on "drop" (Json.succeed msg)