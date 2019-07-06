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
    {
        dir: Direction
        locs: Location list
    }

type SnakeTail = Location * Snake

type Model =
    {
        mouseLocs: Set<Location>
        snake: Snake
        tickInterval: int64
    }

let init () =
    {
        mouseLocs = Set.empty
        snake =
            let loc = {x=gridWidth/2;y=gridHeight/2}
            { dir=Right; locs=[loc] }
        tickInterval=100L
    }

type Message = 
    | Tick
    | ChangeDir of Direction

let moveInDir loc dir =
    match dir with
    | Up -> {x=loc.x; y=(loc.y-1+gridHeight)%gridHeight}
    | Down -> {x=loc.x; y=(loc.y+1)%gridHeight}
    | Left -> {x=(loc.x-1+gridWidth)%gridWidth; y=loc.y}
    | Right -> {x=(loc.x+1)%gridWidth; y=loc.y}

let rec advanceSnakeImpl oldSnake newSnake =
    match oldSnake with
    | [x;y] -> newSnake @ [x]
    | x::xs -> advanceSnakeImpl xs (newSnake @ [x])
    | _ -> newSnake

let advanceSnake model =
    let snakeLocs =
        match model.snake.locs with
        | x::xs ->
            advanceSnakeImpl xs [moveInDir x model.snake.dir]
        | _ -> model.snake.locs
    printfn "snake length = %i" (List.length snakeLocs)
    let snake = {locs=snakeLocs; dir=model.snake.dir}
    {model with snake=snake}

let update message model =
    match message with
    | Tick -> advanceSnake model
    | ChangeDir dir ->
        {model with snake={model.snake with dir=dir}}

let mutable lastTick = 0L // we use a mutable tick counter here in order to ensure precision

let rec visSnake locs (viewables: Viewable list) =
    match locs with
    | x::xs ->
        let rect = colour Colour.Black (tiledim, tiledim) (x.x*tiledim, x.y*tiledim)
        visSnake xs (rect::viewables)
    | [] -> viewables

let view model dispatch =
    (visSnake model.snake.locs []) @ [
        // game controls
        yield onkeydown Keys.Left (fun () -> dispatch (ChangeDir Left))
        yield onkeydown Keys.Right (fun () -> dispatch (ChangeDir Right))
        yield onkeydown Keys.Up (fun () -> dispatch (ChangeDir Up))
        yield onkeydown Keys.Down (fun () -> dispatch (ChangeDir Down))

        // by placing the below code in a viewable function, it will get evaluated on every game draw
        // This can be more effective than using an Elmish subscription, especially if smoothness is needed
        yield onupdate (fun inputs ->
            // check to see if a drop tick is due
            // let interval = if inputs.keyboardState.IsKeyDown Keys.Down then 100L else model.tickInterval
            if (inputs.totalGameTime - lastTick) >= model.tickInterval then
                lastTick <- inputs.totalGameTime
                dispatch Tick)
    ]
