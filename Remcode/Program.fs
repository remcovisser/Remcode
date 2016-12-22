module Program

open System
open Parser
open System.IO



let programInput = File.ReadAllText("program.remcode");
let program = getProgramContent programInput

let programOutput = parser program 0

Console.ReadLine();