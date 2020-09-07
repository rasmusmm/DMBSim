namespace DMBSim

open Avalonia.FuncUI.DSL
open Avalonia.FuncUI

module Grid =
    open Avalonia.FuncUI.Types
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open System
    open System.IO
    let baseDirectory = __SOURCE_DIRECTORY__
    let baseDirectory' = Directory.GetParent(baseDirectory)
    let filePath = "DMBSim\commands.txt"
    let fullPath = Path.Combine(baseDirectory'.FullName, filePath)
    type Msg =
        | MoveChem of GridPosition * GridPosition * Chemical
        | AddChem of  GridPosition * Chemical
        | RemoveChem of GridPosition * Chemical
        | ImportProcedure of string

    let update (msg:Msg) (grid:GridModel):GridModel =
        match msg with
        | MoveChem (pos,dest,chem) -> GridModel.moveChem (pos,dest,chem) (grid)
        | AddChem (dest,chem) -> GridModel.setElectrode (dest,GridModel.addChem(GridModel.getElectrode dest grid,chem)) (grid)
        | RemoveChem (dest,chem) -> GridModel.setElectrode (dest,GridModel.removeChem(GridModel.getElectrode dest grid,chem)) (grid)
        | ImportProcedure (path) -> GridModel.ImportProcedure (path) (grid)
    let view (grid: GridModel) (dispatch: Msg -> unit) : IView =
        DockPanel.create[
            DockPanel.children[
                Button.create [
                            Button.dock Dock.Bottom
                            Button.background "#d35400"
                            Button.onClick ((fun _ -> ImportProcedure (fullPath)  |> dispatch), SubPatchOptions.Always)
                            Button.content "Import Procedure"
                ]|> generalize
                UniformGrid.create [
                    UniformGrid.columns grid.Width
                    UniformGrid.rows grid.Height
                    UniformGrid.children (
                        grid.Electrodes
                        |> Array2D.flati
                        |> Array.map (fun (x, y, electrode) ->
                            let electrodePosition = { x = x; y = y }
                            
                            Button.create [
                                match electrode.ChemList with
                                | [] ->
                                    yield Button.onClick ((fun _ -> AddChem (electrodePosition,("test",1.1)) |> dispatch), SubPatchOptions.OnChangeOf electrodePosition)
                                    yield Button.background "gray"
                                | _ ->
                                    yield Button.onClick ((fun _ -> RemoveChem (electrodePosition,("test",1.1))  |> dispatch), SubPatchOptions.OnChangeOf electrodePosition)
                                    yield Button.background "green"
                                
                            ] |> generalize                     
                        )
                        |> Array.toList
                    )        
        ]
        ]]
        |> generalize
        