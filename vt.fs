module vt

open System
open System.Runtime.InteropServices
open Tmds.Linux

[<DllImport("libc")>] extern void fflush(nativeint file)
[<DllImport("libc")>] extern void printf(string str)
[<DllImport("libc")>] extern nativeint dlopen (string name, int code)
[<DllImport("libc")>] extern nativeint dlsym (nativeint _module, string symbol)
[<DllImport("stdout")>] extern nativeint get_stdout()

type io =
    static member stdin = LibC.STDIN_FILENO
    static member stdout = get_stdout ()
    
let inline fgetc _ = (Console.ReadKey()).KeyChar

[<DllImport("libc")>] extern int atoi(char[] str)
module Terminal =
    let mutable saved = Unchecked.defaultof<termios>
    let disableIcanon () : unit =
        let mutable term = Unchecked.defaultof<termios>
        LibC.tcgetattr(0, &&term)
        LibC.tcgetattr(0, &&saved)
        term.c_lflag <- ~~~(LibC.ICANON ||| LibC.ECHO)
        LibC.tcsetattr(0, int LibC.TCSANOW, &&term)
        ()
    let restoreState () : int =
        LibC.tcsetattr(0, int LibC.TCSANOW, &&saved)
module Cursor =
    let move (x: int) (y: int) : int =
        printf $"\x1b[{y};{x}f"
        // printf $"\x1b[{x}G"
        fflush io.stdout
        0
module Screen =
    let clear () =
        printf "\x1b[2J"
        printf "\x1b[H"
        fflush io.stdout
        Cursor.move 0 0
    let size () =
        // https://stackoverflow.com/a/23370070/1234403
        let mutable w = Unchecked.defaultof<winsize>
        LibC.ioctl(LibC.STDOUT_FILENO, LibC.TIOCGWINSZ, NativeInterop.NativePtr.toVoidPtr &&w)
        int w.ws_col, int w.ws_row
module color =
    let setForeground r g b = printf $"\x1b[38;2;{r};{g};{b}m"
    let setBackground r g b = printf $"\x1b[48;2;{r};{g};{b}m"
    let sixelChar (a: int) (b: int) (c: int) (d: int) (e: int) (f: int) : char =
        char (63 + a + (b * 1) + (c * 2) + (d * 4) + (e * 8) + (f * 32))
    let rgb (deg: int) : int * int * int =
        let deg = deg % 360
        let v = 1.0
        let saturation = 1.0
        let c = v * saturation
        let factor = (double deg) / 60.0
        let mutable remainder = factor
        // Having troubles with float modulus in other Fable targets
        while remainder - 2.0 > 0 do
            remainder <- remainder - 2.0
        if remainder < 1.0 then
            remainder <- 1.0 - remainder
        else
            remainder <- remainder - 1.0
        let x = c * (1.0 - remainder)
        // TODO let m = v - c
        let color = int (c * 255.0)
        let dir = int (x * 255.0)
        if deg < 60 then
            (color, dir, 0)
        elif deg < 120 then
            (dir, color, 0)
        elif deg < 120 then
            (dir, color, 0)
        elif deg < 180 then
            (0, color, dir)
        elif deg < 240 then
            (0, dir, color)
        elif deg < 300 then
            (dir, 0, color)
        elif deg <= 360 then
            (color, 0, dir)
        else
            (0, 0, 0)

module Shape =
    let outlineItem (depth: int) (text: string) =
        for i in 1..depth do printf "  "
        printf $"- {text}\n"
    let line cols =
        for i in 1..cols do
            printf "="
        printf "\n"
    let titlebar title =
        let (cols, rows) = Screen.size ()
        line cols
        printfn title
        line cols
    let box cols rows =
        for i in 1..(cols) do
            printf "-"
        printf "\n"
        for i in 1..(rows - 2) do
            printf "|"
            for i in 1..(cols - 2) do
                printf " "
            printf "|"
            printf "\n"
        for i in 1..(cols) do
            printf "-"
        fflush io.stdout
