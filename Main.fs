namespace DMBSim

module Main =
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Elmish
   
    type State =
        { grid : GridModel
          chem : Chemical
          running : bool }
        
    let initialState() =
        { grid = GridModel.constructBlank(10, 10)
          chem = ("",0.0)
          running = false }, Cmd.none

    type Msg =
        | GridMsg of Grid.Msg
        | Start
        | Stop

    let update (msg: Msg) (state: State) : State * Cmd<_>=
        match msg with
        | Start -> { state with running = true }, Cmd.none
        | Stop -> { state with running = false }, Cmd.none
        | GridMsg msg -> if state.running then {state with grid = Grid.update msg state.grid},Cmd.none else state,Cmd.none
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
                DockPanel.create [
                    DockPanel.dock Dock.Right
                    DockPanel.children[
                        AccessText.create[
                            
                            
                        ]
                        
                    ]
                ]
                
                Grid.view state.grid (GridMsg >> dispatch ) 
            ]
        ]       