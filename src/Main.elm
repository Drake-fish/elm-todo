module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time



---- MODEL ----


type alias Task =
    { body : String
    , isComplete : Bool
    , isEditing : Bool
    , completionTime : Int
    , elapsedTime : Int
    , expired : Bool
    }


type alias Model =
    { taskList : List Task
    , field : String
    , addDisabled : Bool
    , editField : String
    , completion : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] "" True "" 0, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix
    | UpdateField String
    | Add
    | ToggleTask String Bool
    | DeleteTask String
    | ToggleEdit String
    | UpdateTask String
    | UpdateEditField String
    | SetDuration String
    | ResetTimer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            ( { model | taskList = List.map updateElapsedTime model.taskList }, Cmd.none )

        ResetTimer body ->
            ( { model | taskList = List.map (updateTimer body) model.taskList }, Cmd.none )

        Add ->
            ( { model
                | taskList = model.taskList ++ [ Task model.field False False model.completion 0 False ]
                , field = ""
                , addDisabled = True
                , completion = 0
              }
            , Cmd.none
            )

        SetDuration duration ->
            ( { model | completion = Maybe.withDefault 0 (String.toInt duration) }, Cmd.none )

        ToggleTask body isCompleted ->
            ( { model | taskList = List.map (updateComplete body) model.taskList }
            , Cmd.none
            )

        ToggleEdit body ->
            ( { model | taskList = List.map (updateEdit body) model.taskList, editField = body }
            , Cmd.none
            )

        UpdateEditField txt ->
            ( { model | editField = txt }, Cmd.none )

        UpdateField txt ->
            ( { model | field = txt, addDisabled = checkDisabled txt }, Cmd.none )

        UpdateTask body ->
            ( { model | taskList = List.map (updateBody body model) model.taskList, editField = "" }, Cmd.none )

        DeleteTask body ->
            ( { model | taskList = List.filter (removeTask body) model.taskList }, Cmd.none )


ifIsTask : Task -> String -> Task -> Task
ifIsTask task body updatedTask =
    if task.body == body then
        updatedTask

    else
        task


updateEdit : String -> Task -> Task
updateEdit body task =
    ifIsTask task
        body
        { task | isEditing = not task.isEditing }


updateTimer : String -> Task -> Task
updateTimer body task =
    ifIsTask task
        body
        { task | elapsedTime = 0, expired = False }


updateBody : String -> Model -> Task -> Task
updateBody body model task =
    ifIsTask task
        body
        { task | body = model.editField, isEditing = False }


updateComplete : String -> Task -> Task
updateComplete body task =
    ifIsTask task
        body
        { task | isComplete = not task.isComplete }


updateElapsedTime : Task -> Task
updateElapsedTime task =
    if task.isComplete || task.expired then
        task

    else
        { task | elapsedTime = task.elapsedTime + 1, expired = isExpired task }


removeTask : String -> Task -> Bool
removeTask body t =
    t.body /= body


checkDisabled : String -> Bool
checkDisabled txt =
    String.length txt < 1


isExpired : Task -> Bool
isExpired { completionTime, elapsedTime } =
    elapsedTime >= completionTime && completionTime /= 0



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateField, placeholder "Type in your stuff", value model.field ]
            []
        , label [] [ text "Due in (min)" ]
        , select [ onInput SetDuration, value (String.fromInt model.completion) ]
            (List.range 0 15 |> List.map intToOption)
        , button
            [ onClick Add, disabled model.addDisabled ]
            [ text "Add" ]
        , h3 [] [ text "To Do List" ]
        , renderList model.taskList
        ]


intToOption : Int -> Html Msg
intToOption v =
    option [ value (String.fromInt v) ] [ text (String.fromInt v) ]


renderList : List Task -> Html Msg
renderList tasks =
    tasks
        |> List.map createLi
        |> ul []


createLi : Task -> Html Msg
createLi ({ body, isComplete, isEditing } as task) =
    li []
        [ input [ type_ "checkbox", checked isComplete, onClick (ToggleTask body <| not isComplete) ] []
        , renderInput task
        , p []
            [ text <|
                if task.expired then
                    "True"

                else
                    "False"
            ]
        ]


renderInput : Task -> Html Msg
renderInput ({ isEditing, body, isComplete } as task) =
    if isEditing then
        div
            []
            [ input [ placeholder body, onInput UpdateEditField ] []
            , button [ class "submit-button", onClick <| UpdateTask body ] [ text "Submit" ]
            , button [ class "edit-button", onClick <| ToggleEdit body ] [ text "Cancel" ]
            ]

    else
        div []
            [ span [ classList [ ( "complete", isComplete ) ] ] [ text body ]
            , timeRemaining task
            , button [ class "remove-button", onClick <| DeleteTask body ] [ text "remove" ]
            , button [ class "edit-button", onClick <| ToggleEdit body ] [ text "edit" ]
            ]


timeRemaining : Task -> Html Msg
timeRemaining task =
    if isExpiredOrComplete task then
        div []
            [ p [ class "expired" ]
                [ text "Your task is now expired!" ]
            , button
                [ onClick (ResetTimer task.body) ]
                [ text "reset timer" ]
            ]

    else
        p [] [ text (calculateTimeRemaining task) ]


isExpiredOrComplete : Task -> Bool
isExpiredOrComplete { elapsedTime, completionTime, isComplete } =
    (elapsedTime >= completionTime && completionTime /= 0) && not isComplete


calculateTimeRemaining : Task -> String
calculateTimeRemaining task =
    if not task.isComplete then
        let
            remainingTime =
                if task.completionTime - task.elapsedTime <= 0 then
                    ""

                else
                    "Time remaining " ++ String.fromInt (task.completionTime - task.elapsedTime) ++ " minutes"
        in
        remainingTime

    else
        "The Task is Complete!"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
