
open System
open Elmish
open Xelmish.Model
open Constants

type Model = Playing of PlayScreen.Model

type Message = PlayScreenMessage of PlayScreen.Message

let init () =
    Playing (PlayScreen.init ()), Cmd.none

let update message model =
    match model, message with
    | Playing playScreen, PlayScreenMessage msg -> 
        let newModel = PlayScreen.update msg playScreen
        Playing newModel, Cmd.none

let view model dispatch =
    match model with
    | Playing playScreen ->
        PlayScreen.view playScreen (PlayScreenMessage >> dispatch)

[<EntryPoint; STAThread>]
let main _ =
    let config = {
        resolution = Windowed (resWidth, resHeight)
        clearColour = Some Colour.Gray
        mouseVisible = true
        assetsToLoad = []
    }

    Program.mkProgram init update view
    |> Program.withConsoleTrace
    |> Xelmish.Program.runGameLoop config
    0
