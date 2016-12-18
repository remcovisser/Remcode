module Parser

open System.Collections.Generic

(*  Return a string Array with all the words of the programm 
    | words are characters sepparted by a space
    | spaces/breaks/tabs are removed *)
let getWords (text: string) =
     text.Split (' ','\n','\r','\t')
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
dictionary.Add("+", -1)
dictionary.Add("-", -1)
dictionary.Add("/", -1)
dictionary.Add("*", -1)
dictionary.Add("while", -1)
dictionary.Add("}", -1)
dictionary.Add("version", 0)

(*  Define the base stack
    | Contains all the data for the program
    | Grows as the program is executed with new data
*)
let stack = new Dictionary<int, obj>()
stack.Add(0, 0.01)


// Math operators
let doMaths number1 number2 operator = 
    match operator with
        | "+" -> number1 + number2
        | "-" -> number1 - number2
        | "/" -> number1 / number2
        | "*" -> number1 * number2

// Statements
let conditionResult var operator result =
    match operator with
    | "=" -> var = result
    | "!=" -> var <> result
    | ">" -> var > result
    | "<" -> var < result

(*
    Parser
*)
let rec parser (words: string[]) (dictionary: Dictionary<string, int>) (next:int) (stack: Dictionary<int, obj>) = 
    // Check if the program is finished
    match words.Length = next with 
        | true -> printfn "\nThe program has been executed" 
        | false ->
            // Set variable used mutiple times in the parser
            let currentWord = words.[next] 
            // Check if current word exists in the dictionary
            match dictionary.ContainsKey currentWord with
                | false -> 
                    printfn "Error: Unknow word: %s" currentWord
                    let next' = next + 1
                    parser words dictionary next' stack
                | true ->
                    let next', dictionary', stack' = 
                        match currentWord with
                            // -- Printing
                            | "print" | "printLine" -> 
                                let result, next' =
                                    match (dictionary.ContainsKey words.[next+1]), (words.[next+1] = "'") with
                                        // single word
                                        | false, false ->
                                             let next' = next + 2
                                             words.[next+1], next'
                                        // string
                                        | false, true ->
                                            let rec findBetween position (value:string) ending = 
                                                let ending' = ending + 1
                                                match words.[position] with
                                                    | "'" -> value, ending'+2
                                                    | _ -> 
                                                        let position' = position + 1
                                                        let value' =
                                                            match value.Length with
                                                                | 0 -> words.[position]
                                                                | _ -> value + " " + words.[position]
                                                        findBetween position' value' ending'
                                            let position = next + 2
                                            let word, next' = findBetween position "" next
                                            word, next'
                                        // single word from stack
                                        | true, false -> 
                                            let key = dictionary.Item words.[next+1]
                                            let value: string = string (stack.Item key)
                                            let next' = next + 2
                                            value, next'
                                        // Skip, this should never happen
                                        | true, true ->
                                            let value = "Unknown data to print"
                                            let next' = next + 2
                                            value, next'
                                // Print the found value
                                match currentWord with 
                                    | "print" -> printf "%s" result
                                    | "printLine" | _ -> printfn "%s" result
                                next', dictionary, stack
                            // -- Variable creation
                            | "var" -> 
                                let next', stack', key = 
                                    match words.[next+2], words.[next+4] with
                                        // Math operator on variable creation
                                        | "=", ("+" | "-" | "*" | "/") ->
                                            match (dictionary.ContainsKey words.[next+3]), (dictionary.ContainsKey words.[next+5]) with
                                                | true, true ->
                                                    let key1 = dictionary.Item words.[next+3]
                                                    let value1 = stack.Item key1 :?> string |> float
                                                    let key2 = dictionary.Item words.[next+5]
                                                    let value2 = stack.Item key2 :?> string |> float
                                                    let result = doMaths value1 value2 words.[next+4]
                                                    let key = stack.Count+1
                                                    stack.Add(key, result)
                                                    next+6, stack, key
                                                | true, false ->
                                                    let key1 = dictionary.Item words.[next+3]
                                                    let value1 = stack.Item key1 :?> string |> float
                                                    let value2 = words.[next+5] |> float
                                                    let result = doMaths value1 value2 words.[next+4]
                                                    let key = stack.Count+1
                                                    stack.Add(key, result)
                                                    next+6, stack, key
                                                 | false, true ->
                                                     let key2 = dictionary.Item words.[next+5]
                                                     let value1 = words.[next+3] |> float
                                                     let value2 = stack.Item key2 :?> string |> float  
                                                     let result = doMaths value1 value2 words.[next+4]
                                                     let key = stack.Count+1
                                                     stack.Add(key, result)
                                                     next+6, stack, key
                                                | false, false ->
                                                    let value1 = words.[next+3] |> float
                                                    let value2 = words.[next+5] |> float
                                                    let result = doMaths value1 value2 words.[next+4]
                                                    let key = stack.Count+1
                                                    stack.Add(key, result)
                                                    next+6, stack, key
                                        // Variable creation with value
                                        | "=", _-> 
                                            let key = stack.Count+1
                                            stack.Add(key, words.[next+3])
                                            next+4, stack, key
                                        // Variable creation without value
                                        | _ , _ -> next+2, stack, -1
                                dictionary.Add(words.[next+1], key)
                                next', dictionary, stack'
                            // -- If statements
                            // -- Loops
                            | "while" ->
                                let item1Key = dictionary.[words.[next+2]]
                                let item1Value = stack.Item item1Key :?> string |> float  
                                let item2Value = words.[next+4] |> float
                                let conditionResult = conditionResult item1Value words.[next+3] item2Value
                                let next' = match conditionResult with
                                    | true -> 
                                        let next' = next + 7
                                        next'
                                    | false -> 
                                        let rec findClosingBracketPosition (words: string[]) next = 
                                            match words.[next] with
                                                | "}" -> 
                                                    let next' = next + 1
                                                    next'
                                                | _ ->
                                                    let next' = next + 1;
                                                    findClosingBracketPosition words next'
                                        let closingBracketPosition = findClosingBracketPosition words next
                                        closingBracketPosition
                                next', dictionary, stack
                            | "}" ->
                                let rec findOpenBracketPosition (words: string[]) next = 
                                    match words.[next] with
                                        | "{" -> 
                                            let next' = next - 6
                                            next'
                                        | _ ->
                                            let next' = next - 1;
                                            findOpenBracketPosition words next'
                                let next' = findOpenBracketPosition words next
                                
                                next', dictionary, stack
                            | _ ->
                                let next', stack', dictionary' =  
                                    match words.[next+1] with
                                        // Change variable data
                                        // Todo: Needs to be refactored
                                        | "=" -> 
                                            let key = stack.Count+1
                                            stack.Add(key, words.[next+2])
                                            dictionary.[words.[next]] <- key
                                            next+3, stack, dictionary
                                        | "+=" ->
                                            let key = stack.Count
                                            let originalValue = stack.Item key :?> string |> float
                                            let inputValue = words.[next+2] |> float 
                                            let modifiedValue = (originalValue + inputValue) |> string
                                            stack.[key] <- modifiedValue
                                            next+3, stack, dictionary
                                        | "-=" ->
                                            let key = stack.Count+1
                                            let originalValue = stack.Item key :?> string |> float
                                            let inputValue = words.[next+2] |> float 
                                            let modifiedValue = originalValue - inputValue
                                            stack.Add(key, modifiedValue)
                                            dictionary.[words.[next]] <- key
                                            next+3, stack, dictionary
                                        | "*=" ->
                                            let key = stack.Count+1
                                            let originalValue = stack.Item key :?> string |> float
                                            let inputValue = words.[next+2] |> float 
                                            let modifiedValue = originalValue * inputValue
                                            stack.Add(key, modifiedValue)
                                            dictionary.[words.[next]] <- key
                                            next+3, stack, dictionary
                                        | "/=" ->
                                            let key = stack.Count+1
                                            let originalValue = stack.Item key :?> string |> float
                                            let inputValue = words.[next+2] |> float 
                                            let modifiedValue = originalValue / inputValue
                                            stack.Add(key, modifiedValue)
                                            dictionary.[words.[next]] <- key
                                            next+3, stack, dictionary
                                        | _ ->
                                            // If all the mathes failed
                                            printfn "\nError: Unknow action based on the word: %s in dictionary" currentWord 
                                            let next' = next + 1
                                            next', stack, dictionary
                                next', dictionary, stack'
                    // Go to the next step in the program
                    parser words dictionary next' stack