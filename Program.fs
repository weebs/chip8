module Program

open System
open chip8


[<EntryPoint>]
let main argv =
    // let romPath = "/home/dave/Downloads/pong.ch8"
    // let romPath = "/home/dave/Downloads/PONG"
    // let romBytes = File.ReadAllBytes(romPath)
    let romText = FSharp.Data.LiteralProviders.TextFile.PONG.Text
    let romBytes = System.Text.Encoding.Unicode.GetBytes(romText)
    let actualBytes = IO.File.ReadAllBytes FSharp.Data.LiteralProviders.TextFile.PONG.Path
    let instructions = 
        romBytes 
        |> Array.chunkBySize 2
        |> Array.map toOpcode
        |> Array.mapi (fun addr instr -> 
            (uint16 (addr * 2) + uint16 512, parseInstruction instr)
        )
        |> Map.ofArray
        // |> Array.filter (fun (addr, instr) -> instr.Recognized())

    let mutable c8 = Chip8.create(romBytes)
    vt.Screen.clear ()
    printf "\x1B[38;2;0;0;0m"
    for j in 0..31 do
        for i in 0..63 do
            vt.Cursor.move i j
    vt.fflush vt.io.stdout
    for i in [0..10000] do
        let instruction = instructions.[c8.PC]
        c8 <- c8 |> applyInstruction instruction |> fun c8 -> { c8 with DelayTimer = Math.Max(0uy, c8.DelayTimer - 1uy); SoundTimer = Math.Max(0uy, c8.SoundTimer - 1uy) }
        System.Threading.Thread.Sleep(2)
        // c8 <- nextC8
    // for (_addr, instruction) in instructions do
    //     let (nextC8, command) = c8 |> applyInstruction instruction
    //     c8 <- nextC8
    0 // return an integer exit code
