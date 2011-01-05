// Silverlight mini-game by Phil Trelford 2010
namespace LightCycles

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media

/// Tracks which keys are down
type KeyState (control:Control) =
    let mutable keysDown = Set.empty
    do  control.KeyDown.Add (fun e -> keysDown <- keysDown.Add e.Key)
    do  control.KeyUp.Add (fun e -> keysDown <- keysDown.Remove e.Key)
    member this.IsKeyDown key = keysDown.Contains key
    member this.IsAnyKeyDown () = keysDown.Count > 0

module Color =
    /// Converts color to integer value
    let toInt (color:Color) = 
        [color.A,24;color.R,16;color.G,8;color.B,0]
        |> List.sumBy (fun (a,b) -> int a <<< b)

/// Player direction
type Direction = Left | Right | Up | Down

/// Player state
type Player (color,startX,startY,startDirection,keys,keyHandler:KeyState) =
    let mutable x, y, direction = startX, startY, startDirection
    let up, down, left, right = keys
    member this.X = x
    member this.Y = y
    member this.Color = color
    member this.ColorValue = color |> Color.toInt
    member this.Keys = keys
    /// Reset player to start values
    member this.Reset() = x <- startX; y <- startY; direction <- startDirection
    /// Updates player position
    member this.Update() =
        // Check keys
        [up,Up; down,Down; left,Left; right,Right]
        |> List.tryFind (fun (key,_) -> keyHandler.IsKeyDown(key))
        |> Option.iter (fun (_,value) ->
            // Ignore suicide moves
            match direction, value with
            | (Left, Right) | (Right, Left) | (Up, Down) | (Down, Up) -> ()
            | _ -> direction <- value
        )
        // Update position with direction
        match direction with
        | Up    -> y <- y - 1
        | Down  -> y <- y + 1
        | Left  -> x <- x - 1
        | Right -> x <- x + 1

/// Game control
type GameControl() as control = 
    inherit UserControl()

    let keys = new KeyState(control)

    let layout = Grid()
    do  for i=1 to 2 do layout.ColumnDefinitions.Add <| ColumnDefinition()
    do  control.Content <- layout

    /// Creates text prompt
    let createPrompt text =
        TextBlock(Text=text,
            Foreground=SolidColorBrush Colors.White,
            HorizontalAlignment=HorizontalAlignment.Center,
            VerticalAlignment=VerticalAlignment.Center)

    let playArea = 480
    let bitmap = Imaging.WriteableBitmap(playArea,playArea)
    do  Image(Source=bitmap) |> layout.Children.Add
    let setPixel (x,y,c) = bitmap.Pixels.[x + (y * playArea)] <- c
    let getPixel (x,y) = bitmap.Pixels.[x + (y * playArea)]
    let clearPlayArea () = 
        Colors.Black |> Color.toInt |> Array.fill bitmap.Pixels 0 (playArea*playArea)
    do  clearPlayArea () 
    
    /// Player array
    let players = 
        [|Player(Colors.Red,playArea/2-20,playArea/2,Down,(Key.Q,Key.A,Key.Z,Key.X),keys)
          Player(Colors.Cyan,playArea/2+20,playArea/2,Up,(Key.P,Key.L,Key.N,Key.M),keys)|]

    /// Display player control values
    do  let panel = StackPanel(Orientation=Orientation.Vertical)
        panel.VerticalAlignment <- VerticalAlignment.Center
        players |> Array.iteri (fun i player ->
            let add s = 
                TextBlock(Text=s,Foreground=SolidColorBrush player.Color)
                |> panel.Children.Add
            let up,down,left,right = player.Keys
            let name = sprintf "Player %i" (i+1) |> add 
            Array.zip [|"Up";"Down";"Left";"Right"|] [|up;down;left;right|]
            |> Array.map (fun (name,key) -> sprintf "%s '%O'" name key)
            |> Array.iter add
        )
        Grid.SetColumn(panel,1)
        panel |> layout.Children.Add

    /// Game update
    let update () =
        players |> Array.iter (fun player -> player.Update())
        players |> Array.tryFindIndex (fun player -> 
            let x, y = (player.X, player.Y)
            let hitWall = x < 0 || x >= playArea || y < 0 || y >= playArea
            if hitWall then true
            else
                let bgColor = getPixel(x, y)
                setPixel (x, y, player.ColorValue)
                players |> Array.exists (fun p -> bgColor = p.ColorValue)
        )

    /// Game state machine
    let gameState = seq {
        while true do
            players |> Array.iter (fun p -> setPixel(p.X,p.Y,p.ColorValue))
            // Game loop
            let loser = ref None
            while (loser := update(); loser.Value.IsNone) do
                bitmap.Invalidate()
                yield ()
            // Game over
            let winner = if loser.Value.Value = 0 then 2 else 1
            let mess = sprintf "Game Over\r\nPlayer %d Wins" winner |> createPrompt
            layout.Children.Add mess
            for i = 1 to 200 do yield ()
            layout.Children.Remove mess |> ignore
            clearPlayArea()
            players |> Array.iter (fun player -> player.Reset())
            // Wait for key press
            let prompt = "Press any key to start" |> createPrompt
            layout.Children.Add prompt
            while not <| keys.IsAnyKeyDown() do yield ()
            layout.Children.Remove prompt |> ignore
        }

    /// Run game
    let runGame () =
        let state = gameState.GetEnumerator()
        let rate = TimeSpan.FromSeconds(1.0/50.0)
        let lastUpdate = ref DateTime.Now
        let residual = ref (TimeSpan())
        CompositionTarget.Rendering.Add (fun x -> 
            let now = DateTime.Now
            residual := !residual + (now - !lastUpdate)
            while !residual > rate do
                state.MoveNext() |> ignore
                residual := !residual - rate
            lastUpdate := now
        )

    // Start game on mouse click
    do  let prompt = createPrompt "Click to Start"
        layout.Children.Add prompt
        let disposable = ref null
        disposable := 
            control.MouseLeftButtonUp
            |> Observable.subscribe (fun _ ->
                disposable.Value.Dispose()
                layout.Children.Remove prompt |> ignore
                runGame()
            )

/// Silverlight application startup object
type App() as this =
    inherit Application()
    do  this.Startup.AddHandler(fun o e -> this.RootVisual <- GameControl())