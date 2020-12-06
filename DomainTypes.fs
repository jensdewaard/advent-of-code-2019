namespace Advent

type Mass = int
type Fuel = int

module IntCode =
    type Program = list<int>
    type ProgramCounter = int

    let ValueAt (i: int) (p: Program): int = p.[i]

    let UpdateAt (p : Program) k v : Program =
        List.mapi (fun i v' -> if k = i then v else v') p

    let rec Eval ctr (program : Program) : Program =
        let opcode = program.[ctr]
        match opcode with
        | 1 ->
            let loc1 = program.[ctr + 1]
            let loc2 = program.[ctr + 2]
            let out = program.[ctr + 3]
            let dat1 = program.[loc1]
            let dat2 = program.[loc2]
            let prog' = UpdateAt program out (dat1 + dat2)
            Eval (ctr + 4) prog'
        | 2 ->
            let loc1 = program.[ctr + 1]
            let loc2 = program.[ctr + 2]
            let out = program.[ctr + 3]
            let dat1 = program.[loc1]
            let dat2 = program.[loc2]
            let prog' = UpdateAt program out (dat1 * dat2)
            Eval  (ctr + 4) prog'
        | any -> program

    let ToString (p: Program): string = p |> Seq.map string |> String.concat ","
