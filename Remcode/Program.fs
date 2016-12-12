module Program

open System
open Parser
open System.IO



let programInput = File.ReadAllText("program.remcode");
let program = getWords programInput

let programOutput = parser program dictionary 0 stack

Console.ReadLine();
