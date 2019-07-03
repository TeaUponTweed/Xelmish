module PlayScreen

open Xelmish.Model
open Xelmish.Viewables
open Elmish
open Constants

let startPos = ((gridWidth / 2) - 1, gridWidth/2)
let random = System.Random ()

type Location = { x: int; y: int }
   
type Direction =
    | Up
    | Down
    | Left
    | Right

type Snake =
    | Head of Location * Direction
    | Body  of Location * Snake

type SnakeTail = Location * Snake

type Model = {
    mouseLocs: Set<Location>
    snake: SnakeTail
}

let init () = 
    {
        mouseLocs = Set.empty
        snake =
            let loc = {x=gridWidth/2;y=gridHeight/2}
            let head = Head loc
            let tail = SnakeTail ({x=gridWidth/2-1;y=gridHeight/2}, head)
            tail
    }

type Message = 
    | Tick
    | ChangeDir of Direction

let moveHead loc dir =
    match dir with
    | Up -> Head ({x=loc.x; y=(loc.y-1)%gridHeight}, Up)
    | Down -> Head ({x=loc.x; y=(loc.y+1)%gridHeight}, Down)
    | Left -> Head ({x=(loc.x-1)%gridWidth; y=loc.y}, Left)
    | Right -> Head ({x=(loc.x+1)%gridWidth; y=loc.y}, Right)

let advanceSnake model =
    let (_, next) = model.snake
    match next with
    | Head (loc, dir) ->
        SnakeTail (loc, moveHead loc dir)
    | Body (loc, next) ->
        SnakeTail (loc, next)

let update message model =
    match message with
    | Tick -> advanceSnake model
    | ChangeDir dir -> changeSnakeDirection dir model

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let view model dispatch =
    [
        // main falling blocks area
        let blockTiles = tilesForModel model |> Set.ofList
        for x = 0 to gridWidth - 1 do
            for y = 0 to gridHeight - 1 do
                let tx, ty = padding + x * tiledim, padding + y * tiledim
                if blockTiles.Contains (x, y) then
                    yield colour model.shapeType.colour (tiledim, tiledim) (tx, ty)
                else
                    match Map.tryFind (x, y) model.staticBlocks with
                    | Some c ->
                        yield colour c (tiledim, tiledim) (tx, ty)
                    | _ ->
                        yield colour Colour.WhiteSmoke (tiledim, tiledim) (tx, ty)

        // preview window for next shape
        let previewStart = (padding * 2) + (tiledim * gridWidth)
        let previewPos = if model.nextShapeType.rotations.Length = 1 then (2, 1) else (1, 1)
        let nextBlockTiles = tilesFor previewPos model.nextShapeType 0 |> Set.ofList
        for x = 0 to 5 do
            for y = 0 to 3 do
                let tx, ty = previewStart + x * tiledim, padding + y * tiledim
                if nextBlockTiles.Contains (x, y) then
                    yield colour model.nextShapeType.colour (tiledim, tiledim) (tx, ty)
                else
                    yield colour Colour.WhiteSmoke (tiledim, tiledim) (tx, ty)

        // score and line text
        let text = text "connection" 25. Colour.White (-0.5, 0.)
        let textMid = (padding * 2) + (tiledim * (gridWidth + 3))
        let textTop = (padding * 2) + (tiledim * 5)
        yield text (sprintf "lines: %i" model.lines) (textMid, textTop)
        yield text (sprintf "score: %i" model.score) (textMid, textTop + (tiledim + padding))        
        yield text (sprintf "level: %i" (level model.score)) (textMid, textTop + (tiledim + padding) * 2)

        // game controls
        yield onkeydown Keys.Left (fun () -> dispatch Left)
        yield onkeydown Keys.Right (fun () -> dispatch Right)
        yield onkeydown Keys.Up (fun () -> dispatch Rotate)
        yield onkeydown Keys.Escape exit

        // by placing the below code in a viewable function, it will get evaluated on every game draw
        // This can be more effective than using an Elmish subscription, especially if smoothness is needed
        yield onupdate (fun inputs ->
            // check to see if a drop tick is due
            let interval = if inputs.keyboardState.IsKeyDown Keys.Down then 100L else model.tickInterval
            if (inputs.totalGameTime - lastTick) >= interval then
                lastTick <- inputs.totalGameTime
                dispatch Tick)
    ]