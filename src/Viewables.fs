﻿module Xelmish.Viewables

open Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Viewable =
| Colour of colour: Colour * size: (int * int) * pos: (int * int)
| Image of key: string * colour: Colour * size: (int * int) * pos: (int * int)
| Text of text: string * font: string * size: float * colour: Colour * origin: (float * float) * pos: (int * int)
| Clickable of event: (Unit -> Unit) * size: (int * int) * pos: (int * int)
| Layout of rowDefinitions: Definition list * colDefinitions: Definition list * cells: Cell list * size: (int * int) * pos: (int * int)
and Definition = Exact of int | Min of int | Percent of float | Even
and Cell = 
    | TextCell of row: int * col: int * content: ((int * int) -> Viewable)
    | RectCell of row: int * col: int * content: ((int * int) -> (int * int) -> Viewable)

let colour colour size pos = Colour (colour, size, pos)
let image key colour size pos = Image (key, colour, size, pos)
let text font size colour origin text pos = Text (text, font, size, colour, origin, pos)
let clickable event size pos = Clickable (event, size, pos)
let layout rows cols cells size pos = Layout (rows, cols, cells, size, pos)

let private vector2 x y = Vector2(float32 x, float32 y)
let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let private viewablesFrom rows cols cells size pos =
    // take total size
        // remove exacts and percents
        // if remainder divided is less than mins, remove mins
        // set the rest to divided
    []

let rec internal renderViewable (spriteBatch: SpriteBatch) gameState viewable =
    match viewable with
    | Colour (colour, (width, height), (x, y)) ->
        spriteBatch.Draw(gameState.whiteTexture, xnaRect x y width height, xnaColor colour)
    | Image (key, colour, (width, height), (x, y)) ->
        spriteBatch.Draw(gameState.textures.[key], xnaRect x y width height, xnaColor colour)
    | Text (text, font, size, colour, (ox, oy), (x, y)) ->
        let font = gameState.fonts.[font]
        let measured = font.MeasureString (text)
        let scale = let v = float32 size / measured.Y in Vector2(v, v)
        let origin = Vector2(float32 (ox % 1.) * measured.X * scale.X, float32 (oy % 1.) * measured.Y * scale.Y)
        let position = Vector2.Add(origin, vector2 x y)
        spriteBatch.DrawString(font, text, position, xnaColor colour, 0.f, Vector2.Zero, scale, SpriteEffects.None, 0.f)
    | Clickable (event, (width, height), (x, y)) ->
        if (gameState.mouseState.X, gameState.mouseState.Y) ||> isInside x y width height then
            if gameState.mouseState.LeftButton = ButtonState.Pressed 
            && gameState.lastMouseState.LeftButton <> ButtonState.Pressed then
                event ()
    | Layout (rows, cols, cells, size, pos) ->
        viewablesFrom rows cols cells size pos
        |> List.iter (renderViewable (spriteBatch: SpriteBatch) gameState)