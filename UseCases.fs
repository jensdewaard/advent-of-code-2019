namespace Advent

module General =
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