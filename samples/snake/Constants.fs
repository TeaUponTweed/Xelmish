module Constants

open Xelmish.Model

let highScoreFile = "./highscore.txt"

let gridWidth = 64
let gridHeight = 64
let padding = 30
let tiledim = 8
let resWidth = padding + (tiledim * gridWidth) + padding + (tiledim * 6) + padding
let resHeight = padding + (tiledim * gridHeight) + padding
