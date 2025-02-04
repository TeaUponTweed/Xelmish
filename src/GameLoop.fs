module internal Xelmish.GameLoop

open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Media
open Model

/// GameLoop is an inherited implementation of XNA's Game class
type GameLoop (config: GameConfig) as this = 
    inherit Game ()

    let graphics = new GraphicsDeviceManager (this)
    
    // These are set during LoadContent.
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable assets = Unchecked.defaultof<_>
    
    // This is set and updated every Update (60 times a second).
    let mutable inputs = {
        keyboardState = Unchecked.defaultof<_>
        lastKeyboardState = Unchecked.defaultof<_>
        mouseState = Unchecked.defaultof<_>
        lastMouseState = Unchecked.defaultof<_>
        gameTime = Unchecked.defaultof<_>
    }

    // These two collections are set by the Elmish setState call.
    let mutable updatable: (Inputs -> Unit) list = []
    let mutable drawable: (LoadedAssets -> Inputs -> SpriteBatch -> Unit) list = []

    do 
        // Presently Xelmish only supports windowed - fullscreen will come eventually (tm).
        match config.resolution with
        | Windowed (w, h) -> 
            graphics.PreferredBackBufferWidth <- w
            graphics.PreferredBackBufferHeight <- h

        // This draws the hardware mouse if true. 
        // Otherwise you will need to provide your own cursor graphic (if appropriate).
        this.IsMouseVisible <- config.mouseVisible
        
        // This makes draw run at monitor fps, rather than 60fps.
        graphics.SynchronizeWithVerticalRetrace <- true 
        
    /// Used by Xelmish with the Elmish setState. 
    /// Viewables from the Elmish components are accepted 
    /// and assigned internally here for update and drawing
    member __.View
        with set value = 
            let rec splitter updatableAcc drawableAcc =
                function
                | [] -> 
                    updatable <- updatableAcc
                    drawable <- drawableAcc
                | (OnUpdate f)::rest -> splitter (f::updatableAcc) drawableAcc rest
                | (OnDraw f)::rest -> splitter updatableAcc (f::drawableAcc) rest
            // We split the viewables by their DU type to be more efficient during draw/update
            splitter [] [] (List.rev value)

    override __.LoadContent () = 
        spriteBatch <- new SpriteBatch (graphics.GraphicsDevice)

        // Assets are loaded into a reference record type (LoadedAssets) here.
        // Both file based and content pipeline based resources are accepted. 
        let loadIntoAssets assets loadable =
            match loadable with
            | FileTexture (key, path) -> 
                use stream = File.OpenRead path
                let texture = Texture2D.FromStream (this.GraphicsDevice, stream)
                { assets with textures = Map.add key texture assets.textures }
            | PipelineTexture (key, path) ->
                let texture = this.Content.Load<Texture2D> path
                { assets with textures = Map.add key texture assets.textures }
            | PipelineFont (key, path) -> 
                let font = this.Content.Load<SpriteFont> path
                { assets with fonts = Map.add key font assets.fonts }
            | FileSound (key, path) -> 
                use stream = File.OpenRead path
                let sound = SoundEffect.FromStream stream
                { assets with sounds = Map.add key sound assets.sounds }
            | PipelineSound (key, path) ->
                let sound = this.Content.Load<SoundEffect> path
                { assets with sounds = Map.add key sound assets.sounds }
            | FileMusic (key, path) -> 
                let uri = new System.Uri (path, System.UriKind.RelativeOrAbsolute)
                let music = Song.FromUri (key, uri)
                { assets with music = Map.add key music assets.music }
            | PipelineMusic (key, path) ->
                let music = this.Content.Load<Song> path
                { assets with music = Map.add key music assets.music }

        let loadedAssets = 
            { whiteTexture = new Texture2D (this.GraphicsDevice, 1, 1)
              textures = Map.empty 
              fonts = Map.empty 
              sounds = Map.empty 
              music = Map.empty }
        // For rendering pure colour, rather than requiring the user load a colour texture,
        // we create one. It is set with a single white pixel, that can be resized and coloured as needed.
        loadedAssets.whiteTexture.SetData<Color> [| Color.White |]
        assets <- List.fold loadIntoAssets loadedAssets config.assetsToLoad

    override __.Update gameTime =
        // Update inputs. Last keyboard and mouse state are preserved so changes can be detected.
        inputs <- 
            {   lastKeyboardState = inputs.keyboardState
                keyboardState = Keyboard.GetState ()
                lastMouseState = inputs.mouseState
                mouseState = Mouse.GetState ()
                gameTime = gameTime }

        try
            for updateFunc in updatable do 
                updateFunc inputs
        with
            // Quit game is a custom exception used by xelmish 
            // components to tell the game to quit gracefully.
            | :? QuitGame -> __.Exit()

    override __.Draw gameTime =
        Option.iter this.GraphicsDevice.Clear config.clearColour

        // By default, a spritebatch doesn't draw until its .End() method is called.
        // Setting the sort mode to immediate changes this so they are drawn as called, which allows us to
        // change the sampler state (e.g. for pixel graphics vs text) between different sprite calls.
        // A slight performance hit but an improvement in rendering quality, when mixing pixel graphics and smoothed text.
        spriteBatch.Begin (sortMode = SpriteSortMode.Immediate)

        for drawFunc in drawable do 
            drawFunc assets inputs spriteBatch

        spriteBatch.End ()
