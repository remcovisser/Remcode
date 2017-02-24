module Program

open System
open Parser
open System.IO

let programInput = File.ReadAllText("test.remcode");
let program = getProgramContent programInput
let programOutput = parser program 0

Console.ReadLine();