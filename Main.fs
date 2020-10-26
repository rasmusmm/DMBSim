namespace DMBSim

module Main =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Elmish
    open Avalonia.Layout
   
    type State =
        { 
          grid : GridModel
          running : bool
        }
        
    let initialState() =
        { grid = GridModel.constructBlank(10, 10)
          running = false }, Cmd.none

    let stringToGP (input : string) : GridPosition =
        let poslist = input.Split [|','|] |> Array.toList
        let newGP = {GridPosition.x = poslist.[0] |> int ;GridPosition.y = poslist.[1] |> int}
        newGP

    let GPToString (input:GridPosition) : string =
        sprintf "%d,%d" input.x input.y

    let stringToChemical (input : string) : Chemical =
        let chemList = input.Split [|','|] |> Array.toList
        let newChem = (chemList.[0],chemList.[1] |> float)
        newChem

    type Msg =
        | GridMsg of Grid.Msg
        | Start
        | Stop

    let update (msg: Msg) (state: State) : State * Cmd<_>=
        match msg with
        | Start -> { state with running = true }, Cmd.none
        | Stop -> { state with running = false }, Cmd.none
        | GridMsg msg -> if state.running then {state with grid = Grid.update msg state.grid},Cmd.none 
                         else
                            match msg with
                            | Grid.EditProcedure _ -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.ImportProcedure _ -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.ClearGrid -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.EditDest _ -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.EditPos _ -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.EditChem _ -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | Grid.RemoveStep -> {state with grid = Grid.update msg state.grid},Cmd.none
                            | _ -> state,Cmd.none


    let view (state: State) (dispatch: Msg -> unit) =
        DockPanel.create [
            DockPanel.children [
                Button.create [
                    Button.isVisible (not state.running)
                    Button.dock Dock.Bottom
                    Button.background "#16a085"
                    Button.onClick (fun _ -> Start|> dispatch)
                    Button.content "start"
                ]                
                Button.create [
                    Button.isVisible state.running
                    Button.dock Dock.Bottom
                    Button.background "#d35400"
                    Button.onClick (fun _ -> Stop |> dispatch)
                    Button.content "stop"
                ]
                
                Grid.view state.grid (GridMsg >> dispatch)
                
                
                
                
            ]
        ]       