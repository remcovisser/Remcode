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
      Value     |> The key of an element on the stack, only applies to variables, is -1 if not a variable
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
dictionary.Add("%", -1)
dictionary.Add("while", -1)
dictionary.Add("endWhile", -1)
dictionary.Add("if", -1)
dictionary.Add("else", -1)
dictionary.Add("endif", -1)
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
        | "%" -> number1 % number2

// Statements
let statement var operator result =
    match operator with
        | "=" -> var = result
        | "!=" -> var <> result
        | ">" -> var > result
        | ">=" -> var >= result
        | "<" -> var < result
        | "<=" -> var <= result

let updateDictionaryAndOrStack word value =
    match dictionary.ContainsKey word with
        // Update variable
        | true ->
            let key = dictionary.Item word
            stack.Item key <- value
        // Create variable
        | false -> 
            let key = stack.Count+1
            stack.Add(key, value)
            dictionary.Add(word, key)
    ()

// Get the value based on the input
let getValue word =
    match (dictionary.ContainsKey word) with
        | true -> 
            let key = dictionary.Item word
            let value = stack.Item key |> string
            value
        | false ->
            word
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
                                let value, next' =
                                    match (words.[next+1] = "'") with
                                        // single word
                                        | false ->
                                             let next' = next + 2
                                             let value = getValue words.[next+1]
                                             value, next'
                                        // string
                                        |  true ->
                                            let rec findBetween position (value:string) ending = 
                                                let ending' = ending + 1
                                                match words.[position] with
                                                    | "'" -> value, ending'+2
                                                    | _ -> 
                                                        let position' = position + 1
                                                        let value' =
                                                            match value.Length with
                                                                | 0 -> words.[position]
                                                                | _ -> 
                                                                    match value with
                                                                        | "&space" -> " " + words.[position]
                                                                        | _ -> value + " " + words.[position]
                                                        findBetween position' value' ending'
                                            let position = next + 2
                                            let value, next' = findBetween position "" next
                                            value, next'
                                // Print the found value
                                match currentWord with 
                                    | "print" -> printf "%s" value
                                    | "printLine" | _ -> printfn "%s" value
                                next', dictionary, stack
                            // -- Variable creation
                            | "var" -> 
                                let next' = 
                                    match words.[next+2], words.[next+4] with
                                        // Math operator on variable creation
                                        | "=", ("+" | "-" | "*" | "/" | "%") ->
                                            let value1 = getValue words.[next+3] |> float
                                            let value2 = getValue words.[next+5] |> float
                                            let operator = words.[next+4]
                                            let result = doMaths value1 value2 operator
                                            updateDictionaryAndOrStack words.[next+1] result
                                            let next' = next+6   
                                            next'
                                        // Variable creation with value
                                        | "=", _-> 
                                            updateDictionaryAndOrStack words.[next+1] words.[next+3]
                                            let next' = next+4
                                            next'
                                        // Variable creation without value
                                        | _ , _ -> 
                                            updateDictionaryAndOrStack words.[next+1] -1
                                            let next' = next+2
                                            next'
                                next', dictionary, stack
                            // -- If statements'
                            | "if" ->
                                let item1 = getValue words.[next+1] |> float
                                let item2 = getValue words.[next+3] |> float
                                let operator = words.[next+2]
                                let conditionResult = statement item1 operator item2
                                let next' =
                                    match conditionResult with
                                        | true -> 
                                            let next' = next + 4
                                            next'
                                        | false -> 
                                            // Find the beginning of the else block
                                            let rec findElseBlock (words: string[]) next = 
                                                match words.[next] with
                                                    | "else" -> 
                                                        let next' = next + 1
                                                        next'
                                                    | _ ->
                                                        let next' = next + 1;
                                                        findElseBlock words next'
                                            let endWhilePosition = findElseBlock words next
                                            endWhilePosition
                                next', dictionary, stack
                            | "else" ->
                                let rec findEndIf (words: string[]) next = 
                                    match words.[next] with
                                        | "endif" -> 
                                            let next' = next + 1
                                            next'
                                        | _ ->
                                            let next' = next + 1;
                                            findEndIf words next'
                                let next' = findEndIf words next
                                next', dictionary, stack
                            | "endif" ->
                                let next' = next + 1
                                next', dictionary, stack
                            // -- Loops
                            | "while" ->
                                let item1 = getValue words.[next+1] |> float
                                let item2 = getValue words.[next+3] |> float
                                let operator = words.[next+2]
                                let conditionResult = statement item1 operator item2
                                let next' =
                                    match conditionResult with
                                        | true -> 
                                            let next' = next + 4
                                            next'
                                        | false -> 
                                            // Loop is done, go to the next step after the loop
                                            let rec findEndOfWhilePosition (words: string[]) next = 
                                                match words.[next] with
                                                    | "endWhile" -> 
                                                        let next' = next + 1
                                                        next'
                                                    | _ ->
                                                        let next' = next + 1;
                                                        findEndOfWhilePosition words next'
                                            let endWhilePosition = findEndOfWhilePosition words next
                                            endWhilePosition
                                next', dictionary, stack
                            | "endWhile" ->
                                // If at the end of a loop go back to see if the loop is finished
                                let rec findWhilePosition (words: string[]) next = 
                                    match words.[next] with
                                        | "while" -> next
                                        | _ ->
                                            let next' = next - 1;
                                            findWhilePosition words next'
                                let next' = findWhilePosition words next              
                                next', dictionary, stack
                            | _ ->
                                let next', stack', dictionary' =  
                                    match words.[next+1] with
                                        // Change variable data
                                        | "=" -> 
                                            let key = stack.Count+1
                                            stack.Add(key, words.[next+2])
                                            dictionary.[words.[next]] <- key
                                            next+3, stack, dictionary
                                        | "+=" | "-=" | "*=" | "/=" ->
                                            let originalValue = getValue words.[next] |> float
                                            let inputValue = getValue words.[next+2] |> float 
                                            let operator = words.[next+1].[0] |> string
                                            let modifiedValue = doMaths originalValue inputValue operator
                                            let key = dictionary.Item words.[next]
                                            stack.[key] <- modifiedValue
                                            next+3, stack, dictionary
                                        | _ ->
                                            printfn "\nError: Unknow action based on the word: %s in dictionary" currentWord 
                                            let next' = next + 1
                                            next', stack, dictionary
                                next', dictionary, stack'
                    // Go to the next step in the program
                    parser words dictionary next' stack