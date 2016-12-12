module Parser

open System.Collections.Generic

(*  Return a string Array with all the words of the programm 
    | words are characters sepparted by a space
    | spaces/breaks are removed *)
let getWords (text: string) =
     text.Split (' ','\n','\r')
     |> Array.filter(fun words -> words = "" |> not)

(*  Define the base dictionary
    | Contains all the know keywords for the program 
    | Grows as the program is executed with new variables
    | Key       |> The unqiue value to indentify the element
      Value     |> A the key of an element on the stack, only applies to variables, is empty string if not a variable
*)
let dictionary = new Dictionary<string, int>()
dictionary.Add("var", -1)
dictionary.Add("=", -1)
dictionary.Add("print", -1)
dictionary.Add("printLine", -1)
dictionary.Add("version", 0)

(*  Define the base stack
    | Contains all the data for the program
    | Grows as the program is executed with new data
*)
let stack = new Dictionary<int, obj>()
stack.Add(0, 0.01)

(*
    Parser
*)
let rec parser (words: string[]) (dictionary: Dictionary<string, int>) (next:int) (stack: Dictionary<int, obj>) = 
    // Check if the program is finished
    match words.Length = next with 
        | true -> printfn "The program has been executed" 
        | false ->
            let currentWord = words.[next] 
            // Check if current word exists in the dictionary
            match dictionary.ContainsKey currentWord with
                | false -> printfn "Error: Unknow word: %s" currentWord
                | true ->
                    let next' = 
                        match currentWord with
                            // -- Printing
                            | "print" | "printLine" -> 
                                let result, next' = 
                                    match (dictionary.ContainsKey words.[next+1]), (words.[next+1] = "'") with
                                        // Unknow in dictionary, single word
                                        | false, false -> words.[next+1], next
                                        // Unknow in dictionary, string
                                        | false, true ->
                                            let rec findBetweenQuotes position (value:string) ending = 
                                                let ending' = ending + 1
                                                match words.[position] with
                                                    | "'" -> value, ending'
                                                    | _ -> 
                                                        let position' = position + 1
                                                        let value' =
                                                            match value.Length with
                                                                | 0 -> words.[position]
                                                                | _ -> value + " " + words.[position]
                                                        findBetweenQuotes position' value' ending'
                                            let position = next + 2
                                            let value2, ending2 = findBetweenQuotes position "" next
                                            value2, ending2
                                        // Known in dictionary, corresponding value from stack
                                        | true, false -> 
                                            let key = dictionary.Item words.[next+1]
                                            let value: string = string (stack.Item key)
                                            value, next
                                // Print the found value
                                match currentWord with 
                                    | "print" ->
                                        printf "%s" result
                                        next'+2
                                    | "printLine" -> 
                                        printfn "%s" result
                                        next'+2
                            // -- Variable creation
                            // -- Variable assignment
                            // -- Basic math opperations
                            | _ -> 
                                printfn "\nError: Unknow action based on the word: %s in dictionary" currentWord 
                                next+1

                    // Go to the next step in the program
                    parser words dictionary next' stack