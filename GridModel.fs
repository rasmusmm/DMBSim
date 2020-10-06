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
    PlainProcedure : string
    SelectedPos : GridPosition
    SelectedDest : GridPosition
    Chem : Chemical
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
        { grid with Electrodes = Array2D.set grid.Electrodes (pos.y-1) (pos.x-1) electrode  }
    
         
    let getElectrode (pos: GridPosition) (grid: GridModel)  : Electrode =
        Array2D.get grid.Electrodes pos.x pos.y
    
    
    let constructBlank (width: int, height: int) : GridModel =
        { Width = width
          Height = height
          Electrodes = Array2D.create width height { Activation = false}
          Droplets = []
          Procedure = []  
          PlainProcedure = ""
          SelectedPos = {x=1;y=1}
          SelectedDest = {x=5;y=5}
          Chem = ("TestChem1",1.2)
        } 
    let DropletValues (droplet : Droplet) =
        (droplet.ChemList,droplet.Pos.x, droplet.Pos.y)
    
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

    let ChemicalToString (chem : Chemical ) : string =
        sprintf "%s,%f" (fst chem) (snd chem)

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
        ((abs(pos.x - dest.x)=1)&&pos.y=dest.y) || ((abs(pos.y - dest.y)=1)&&pos.x=dest.x) 

    //jesus christ
    let moveStep (pos : GridPosition, dest : GridPosition) : string list list =
        let xSteps = [for i in pos.x .. dest.x-1 -> sprintf "MV %i,%i %i,%i" i pos.y (i+1) pos.y ]
        let ySteps = [for i in pos.y .. dest.y-1 -> sprintf "MV %i,%i %i,%i" dest.x i dest.x (i+1)]
        let results = xSteps@ySteps |> List.map (fun sl -> Seq.toList (sl.Split ' ') )
        [["dummystep"]]@results
    let oneMove (pos : GridPosition, dest : GridPosition) (grid : GridModel) : GridModel =
        {grid with Droplets = List.map (fun d -> if d.Pos = pos then {d with Pos = dest} else d) grid.Droplets}

    let moveChem (pos : GridPosition, dest : GridPosition) (grid : GridModel) : GridModel =
        if checkIfNeighbour (pos,dest) then
            let tempGrid = setElectrode (dest,{Activation = true}) (grid) |> setElectrode (pos,{Activation = false})
            {tempGrid with Droplets = List.map (fun d -> if d.Pos = pos then {d with Pos = dest} else d) grid.Droplets}
        else
            let tempProcedure = List.tail grid.Procedure
            {grid with Procedure = moveStep (pos,dest)@tempProcedure}

    let rec plainTextHelper (input : string list, acc : string) : string =
        match input with
        | (s::sl) -> plainTextHelper (sl,acc + " " + s)
        | [] -> acc
        
    let plainTextProcedure (input : string list list) : string =
        match input with
        | [] -> ""
        | _ ->  List.map (fun s -> plainTextHelper(s,"")) input
                 |> List.fold (fun acc s -> acc + s + "\n") ""

    let splitDroplet (pos: GridPosition, dest: GridPosition) (grid : GridModel) : GridModel =
        let newDroplet = List.find (fun droplet -> droplet.Pos = pos) grid.Droplets
        let tempPos = match pos,dest with
                        | pos,dest when pos.x>dest.y || pos.y>dest.y -> {pos with x=pos.x-1;y=pos.y-1}
                        | pos,dest when pos.x>dest.y || pos.y<dest.y -> {pos with x=pos.x-1;y=pos.y+1}
                        | pos,dest when pos.x<dest.y || pos.y>dest.y -> {pos with x=pos.x+1;y=pos.y-1}
                        | pos,dest when pos.x<dest.y || pos.y<dest.y -> {pos with x=pos.x+1;y=pos.y+1}
                        | _ -> {x=0;y=0}
        let newDroplet1 = {newDroplet with ChemList = List.map (fun (s,v)->(s,v*0.5)) newDroplet.ChemList ;Pos = tempPos} 
        let newDroplets = List.map (fun d -> if d.Pos = pos then {d with ChemList = List.map (fun (s,v)->(s,v*0.5)) d.ChemList} else d) grid.Droplets
        let tempGrid = {grid with Droplets = newDroplets @ [newDroplet1]} |> setElectrode (tempPos,{Activation = true})
        //let tempGrid1 = setElectrode (dest,{Activation = true}) (tempGrid)
        //{tempGrid1 with Droplets = List.map (fun d -> if d.Pos = tempPos then {d with Pos = dest} else d) tempGrid1.Droplets}
        moveChem (tempPos,dest) tempGrid


    let removeProcStep (grid:GridModel) : GridModel =
        let tempGrid = {grid with Procedure = List.tail grid.Procedure}
        {tempGrid with PlainProcedure = plainTextProcedure tempGrid.Procedure}

    let handleProcedure (grid:GridModel) : GridModel =
        printfn "Handling Procedure step"
        printfn "%A" grid.Droplets
        printfn "%A" grid.Procedure
        match grid.Procedure with
        | [cmd;pos;dest]::sl when cmd = "MV" -> moveChem (stringToGP pos , stringToGP dest) (grid) |> removeProcStep
        | [cmd;dest;chem]::sl when cmd = "AD" -> let droplet = addChem ((getDroplet (stringToGP dest) (grid)),stringToChemical chem)
                                                 let tempGrid = setElectrode ((stringToGP dest),{Activation = true}) (grid)
                                                 setDroplet ((stringToGP dest),droplet) (tempGrid) |> removeProcStep
        | [cmd;dest;chem]::sl when cmd = "RM" -> let droplet = removeChem ((getDroplet ((stringToGP dest)) (grid)),stringToChemical chem)
                                                 let tempGrid = setElectrode ((stringToGP dest),{Activation = false}) (grid)
                                                 setDroplet ((stringToGP dest,droplet)) (tempGrid) |> removeProcStep
        | [cmd;pos;dest]::sl when cmd = "SP" -> splitDroplet (stringToGP pos,stringToGP dest) (grid)  |> removeProcStep
        | [] -> grid
        | _ -> failwith "Unknown or invalid procedure step."

    let EditProcedure (cmd: string) (grid:GridModel) : GridModel =
        match cmd with
        | "MV" -> {grid with PlainProcedure = grid.PlainProcedure + sprintf "MV %s %s\n" (GPToString grid.SelectedPos) (GPToString grid.SelectedDest);Procedure = grid.Procedure @ [["MV";GPToString grid.SelectedPos;GPToString grid.SelectedDest]]}
        | "AD" -> {grid with PlainProcedure = grid.PlainProcedure + sprintf "AD %s %s\n" (GPToString grid.SelectedDest) (ChemicalToString grid.Chem);Procedure = grid.Procedure @ [["AD";GPToString grid.SelectedDest;ChemicalToString grid.Chem]]}
        | "RM" -> {grid with PlainProcedure = grid.PlainProcedure + sprintf "RM %s %s\n" (GPToString grid.SelectedDest) (ChemicalToString grid.Chem);Procedure = grid.Procedure @ [["RM";GPToString grid.SelectedDest;ChemicalToString grid.Chem]]}
        | "SP" -> {grid with PlainProcedure = grid.PlainProcedure + sprintf "SP %s %s\n" (GPToString grid.SelectedPos) (GPToString grid.SelectedDest);Procedure = grid.Procedure @ [["SP";GPToString grid.SelectedPos;GPToString grid.SelectedDest]]}
        | _ -> grid
    
    let ImportProcedure (path:string) (grid:GridModel) : GridModel =
        printfn "Import Initiated"
        let lines = File.ReadAllLines(path) |> Seq.toList |> List.map (fun x -> Array.toList (x.Split [|' '|]) ) 
        let tempGrid = {grid with Procedure = lines}
        {tempGrid with PlainProcedure = plainTextProcedure grid.Procedure}