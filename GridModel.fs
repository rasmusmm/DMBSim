namespace DMBSim

open System.IO
open System
type Chemical = (string * float)

type Electrode = {
    Activation : bool
}
type Droplet ={
    ChemList : Chemical list
    Pos : GridPosition
}
type GridModel = { 
    Width : int
    Height : int
    Electrodes : Electrode[,]
    Droplets : Droplet list
    Procedure : string list list
}

module GridModel =

    let setDroplet (pos : GridPosition, droplet:Droplet) (grid:GridModel) : GridModel =
        if List.exists (fun d -> d.Pos = pos) grid.Droplets then
            printfn "droplet does exist, updating"
            { grid with Droplets = (List.map (fun d -> if d.Pos = pos then droplet else d ) grid.Droplets)}
            
        else
            printfn "droplet with pos doesnt exist, creating one"
            { grid with Droplets = grid.Droplets @ [droplet]}

    let getDroplet (pos: GridPosition) (grid: GridModel)  : Droplet =
        if List.exists (fun d -> d.Pos = pos) grid.Droplets then
            List.find (fun droplet -> droplet.Pos = pos) grid.Droplets
        else
            printfn "#1"
            {ChemList = [];Pos = pos;}

    let setElectrode (pos : GridPosition, electrode:Electrode) (grid:GridModel) : GridModel =
        { grid with Electrodes = Array2D.set grid.Electrodes pos.x pos.y electrode  }

    let getElectrode (pos: GridPosition) (grid: GridModel)  : Electrode =
        Array2D.get grid.Electrodes pos.x pos.y
    
    let private tryGetElectrode (grid: GridModel, pos: GridPosition) : Electrode option =
        let xInRange = pos.x >= 0 && pos.x < grid.Width
        let yInRange = pos.y >= 0 && pos.y < grid.Height
        if (xInRange && yInRange) then
            try Some (Array2D.get grid.Electrodes pos.x pos.y)
            with _ -> None
        else None
    
    let constructBlank (width: int, height: int) : GridModel =
        { Width = width
          Height = height
          Electrodes = Array2D.create width height { Activation = false}
          Droplets = []
          Procedure = []  
        } 

    let DropletValues (droplet : Droplet) =
        (droplet.ChemList,droplet.Pos.x, droplet.Pos.y)
    
    let stringToGP (input : string) : GridPosition =
        let poslist = input.Split [|','|] |> Array.toList
        let newGP = {GridPosition.x = poslist.[0] |> int ;GridPosition.y = poslist.[1] |> int}
        newGP

    let stringToChemical (input : string) : Chemical =
        let chemList = input.Split [|','|] |> Array.toList
        let newChem = (chemList.[0],chemList.[1] |> float)
        newChem

    let RemoveEmptyChem (chemList : Chemical list) : Chemical list =
        List.filter (fun (s,v) ->  v > 0.0 ) chemList
    
    let addChem (droplet : Droplet, chemical : Chemical) : Droplet =
        if List.exists (fun (s,v) -> s = fst chemical) droplet.ChemList then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,v + (snd chemical)) else (s,v)) droplet.ChemList
            {droplet with ChemList = newChemList}
        else
            printfn "#2"
            {droplet with ChemList = droplet.ChemList @ [chemical]}
    
    let removeChem (droplet : Droplet, chemical : Chemical) : Droplet =
        if (List.exists (fun (s,v) -> s = fst chemical) droplet.ChemList && List.exists (fun (s,v) -> v >= snd chemical) droplet.ChemList)  then
            let newChemList = List.map (fun (s,v) -> if s = fst chemical then (s,(v - (snd chemical))) else (s,v)) droplet.ChemList |>  RemoveEmptyChem
            {droplet with ChemList = newChemList}
        else
            failwith "Failure removing chemical from droplet. Confirm that droplet contains chemical in larger quantity than you are trying to remove."

    let checkIfNeighbour (pos: GridPosition, dest: GridPosition):bool =
        ((pos.x = dest.x+1 || pos.x = dest.x-1 || pos.x = dest.x) && (pos.y = dest.y+1 || pos.y = dest.y-1 || pos.y=dest.y))

    //jesus christ
    let moveStep (pos : GridPosition, dest : GridPosition) : GridPosition list =
        let xStepsNeeded = abs (dest.x - pos.x)
        let yStepsNeeded = abs (dest.y-pos.y)
        
        let xSteps = [for i in pos.x .. dest.x -> {x = i;y = pos.y}]
        let lastX = xSteps.Tail.[0]
        let ySteps = [for i in pos.y .. dest.y -> {lastX with y = i}]
        printfn "%A" (xSteps@ySteps)
        xSteps@ySteps

    let oneMove (pos : GridPosition, dest : GridPosition) (grid : GridModel) : GridModel =
        {grid with Droplets = List.map (fun d -> if d.Pos = pos then {d with Pos = dest} else d) grid.Droplets}

    let moveChem (pos : GridPosition, dest : GridPosition, chemical : Chemical) (grid : GridModel) : GridModel =
        if checkIfNeighbour (pos,dest) then
            {grid with Droplets = List.map (fun d -> if d.Pos = pos then {d with Pos = dest} else d) grid.Droplets}
        else
            {grid with Droplets = List.map (fun d -> if d.Pos = pos then {d with Pos = dest} else d) grid.Droplets}
        

    let splitDroplet (pos: GridPosition, dest: GridPosition, percentage : float) (grid : GridModel) : GridModel =
        let newDroplet = List.find (fun droplet -> droplet.Pos = pos) grid.Droplets
        let newDroplet1 = {newDroplet with ChemList = List.map (fun (s,v)->(s,v*(1.0-percentage))) newDroplet.ChemList ;Pos = dest} 
        let newDroplets = List.map (fun d -> if d.Pos = pos then {d with ChemList = List.map (fun (s,v)->(s,v*percentage)) d.ChemList} else d) grid.Droplets
        {grid with Droplets = newDroplets @ [newDroplet1]}

    let removeProcStep (grid:GridModel) : GridModel =
        {grid with Procedure = List.tail grid.Procedure}

    let handleProcedure (grid:GridModel) : GridModel =
        printfn "Handling Procedure step"
        printfn "%A" grid.Droplets
        match grid.Procedure with
        | [cmd;pos;dest;chem]::sl when cmd = "MV" -> moveChem (stringToGP pos , stringToGP dest,stringToChemical chem) (grid) |> removeProcStep
        | [cmd;dest;chem]::sl when cmd = "AD" -> let droplet = addChem ((getDroplet (stringToGP dest) (grid)),stringToChemical chem)
                                                 setDroplet ((stringToGP dest),droplet) (grid) |> removeProcStep
        | [cmd;dest;chem]::sl when cmd = "RM" -> let droplet = removeChem ((getDroplet ((stringToGP dest)) (grid)),stringToChemical chem)
                                                 setDroplet ((stringToGP dest,droplet)) (grid) |> removeProcStep
        | [cmd;pos;dest;perc]::sl when cmd = "SP" -> splitDroplet (stringToGP pos,stringToGP dest,perc |> float) (grid)  |> removeProcStep
        | _ -> grid
        
    
    let ImportProcedure (path:string) (grid:GridModel) : GridModel =
        printfn "Import Initiated"
        let lines = File.ReadAllLines(path) |> Seq.toList |> List.map (fun x -> Array.toList (x.Split [|' '|]) ) 
        {grid with Procedure = lines}