namespace Advent


module General =
    let readFile filePath = System.IO.File.ReadLines filePath |> String.concat "\n"
    let readLines filePath = System.IO.File.ReadLines(filePath)
  
module Day1 =
    let RequiredFuel (mass: Mass): Fuel = (mass / 3) - 2

    let rec RequiredFuelProper (mass: Mass): Fuel =
        if mass < 0 then
            0
        else
            let f = RequiredFuel(mass)
            f + max (RequiredFuelProper f) 0

    let SolveA (): Fuel =
        let input = General.readLines "input/1"
        Seq.sumBy RequiredFuel (Seq.map int input)

    let SolveB () =
        let input = General.readLines "input/1"
        Seq.sumBy RequiredFuelProper (Seq.map int input)

module Day2 =
    open IntCode 

    let ModifyProgram (noun, verb : int) (prog : Program) =
        prog 
            |> fun x -> UpdateAt x 1 noun 
            |> fun x -> UpdateAt x 2 verb

    let ParseInput(input : string) : Program =
        input.Split ',' |> Seq.map int |> Seq.toList

    let SolveA () =
        let prog = General.readFile("input/2") |> ParseInput |> ModifyProgram (12, 2)
        Eval 0 prog |> ValueAt 0

    let rec cartesian xs ys = 
        match xs, ys with
        | _ , [] -> []
        | [], ys -> []
        | x::xs', _ -> (List.map (fun y -> x, y) ys) @ (cartesian xs' ys)

    let equals (n : int) (m : int) = 
        n = m

    type FunctionMapping = int * int * int

    let printTriple (x, y, z) = sprintf "( %d, %d, %d )" x y z

    let SolveB () =
        let inputProg = General.readFile("input/2") |> ParseInput
        let ns = [1 .. 100]
        let vs = [1 .. 100]
        let inputs = cartesian ns vs
        let fx = inputs |> List.map (fun (x,y) -> (x, y, ModifyProgram (x, y)))
        let results = List.map ((fun (x, y, f) -> (x, y, f inputProg)) >> (fun (x, y, p) -> (x, y, Eval 0 p))) fx
        results 
            |> List.map (fun (x, y, p) -> (x, y, ValueAt 0 p))
            |> List.filter (fun (x, y, n) -> equals n 19690720) 
            |> List.exactlyOne
            |> printTriple
