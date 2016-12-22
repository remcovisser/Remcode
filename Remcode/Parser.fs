module Parser

open System.Collections.Generic

(*  Return a string Array with all the program of the programm 
    | program are characters sepparted by a space
    | spaces/breaks/tabs are removed *)
let getProgramContent (text: string) =
     text.Split (' ','\n','\r','\t')
     |> Array.filter(fun program -> program = "" |> not)

(*  Define the base dictionary
    | Contains all the know keyprogram for the program 
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
dictionary.Add("breakLine", -1)
dictionary.Add("//", -1)
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
        | "=" -> number2
        | "sqrt" -> sqrt number2

// conditions
let condition var operator result =
    match operator with
        | "=" -> var = result
        | "!=" -> var <> result
        | ">" -> var > result
        | ">=" -> var >= result
        | "<" -> var < result
        | "<=" -> var <= result
     
// Specific operators
let arithmeticOperators = ["+"; "-"; "/"; "*"; "%"; "sqrt"]
let relationalOperators = ["="; "!="; ">"; ">="; "<"; ]
let modifyingOperators = [ "="; "+="; "-="; "/="; "*="]
(*
    True: if the variable already exists in the dictionary, only update the stack
    False: If its a new variable, add item on the dictionary and the stack
*)
let updateDictionaryAndOrStack word value =
    match dictionary.ContainsKey word with
        | true ->
            let key = dictionary.Item word
            stack.Item key <- value
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
let rec parser (program: string[]) (next:int) = 
    // Check if the program is finished
    match program.Length = next with 
        | true -> printfn "\nRemcode program has been executed" 
        | false ->
            // Check if current word exists in the dictionary
            match dictionary.ContainsKey program.[next] with
                | false -> 
                    printfn "Error: Unknow word: %s" program.[next]
                    parser program (next+1)
                | true ->
                    let next' = 
                        match program.[next] with
                            // -- Printing
                            | "print" | "printLine" -> 
                                let value, next' =
                                    match (program.[next+1] = "'") with
                                        // single word
                                        | false ->
                                            let value, next' =
                                                match List.contains program.[next+2] arithmeticOperators with                                                
                                                    | true ->
                                                        let value1 = getValue program.[next+1] |> float
                                                        let value2 = getValue program.[next+3] |> float
                                                        let operator = program.[next+2]
                                                        let result = doMaths value1 value2 operator
                                                        let value = result |> string
                                                        value, next+4
                                                    | false ->
                                                         let value = getValue program.[next+1]
                                                         value, next+2
                                            value, next'
                                        // string
                                        |  true ->
                                            let rec findBetween position (value:string) ending = 
                                                let ending' = ending + 1
                                                match program.[position] with
                                                    | "'" -> value, ending'+2
                                                    | _ -> 
                                                        let value' =
                                                            match program.[position] with
                                                                | "&space" -> value + " "
                                                                | _ -> value + " " + program.[position]
                                                        let position' = position + 1
                                                        findBetween position' value' ending'
                                            let position = next + 2
                                            let value, next' = findBetween position "" next
                                            value, next'
                                // Print the found value
                                match program.[next] with 
                                    | "print" -> printf "%s" value
                                    | "printLine" | _ -> printfn "%s" value
                                next'
                            // Break
                            | "breakLine" ->
                                printf "\n"
                                next+1
                            // Comment
                            | "//" ->
                                let rec findBetween position =   
                                    match program.[position] with
                                        | "\\\\" -> position+1
                                        | _ -> 
                                            let position' = position + 1
                                            findBetween position'
                                let next' = findBetween next
                                next'
                            // -- Variable creation
                            | "var" -> 
                                let next' = 
                                    match program.[next+2], List.contains program.[next+4] arithmeticOperators with
                                        // Math operator on variable creation
                                        | "=", true ->
                                            let value1 = getValue program.[next+3] |> float
                                            let value2 = getValue program.[next+5] |> float
                                            let operator = program.[next+4]
                                            let result = doMaths value1 value2 operator
                                            updateDictionaryAndOrStack program.[next+1] result
                                            next+6   
                                        // Variable creation with value
                                        | "=", false -> 
                                            updateDictionaryAndOrStack program.[next+1] program.[next+3]
                                            next+4
                                        // Variable creation without value
                                        | _ , _ -> 
                                            updateDictionaryAndOrStack program.[next+1] -1
                                            next+2
                                next'
                            // -- If condition'
                            | "if" ->
                                let item1, item2, operator, steps = 
                                    match List.contains program.[next+2] relationalOperators  with 
                                        | true ->
                                            let value1 = getValue program.[next+1] |> float
                                            let value2, steps = 
                                                match List.contains program.[next+4] arithmeticOperators with
                                                    // x = y + z  
                                                    | true ->
                                                        let value1 = getValue program.[next+3] |> float
                                                        let value2 = getValue program.[next+5] |> float
                                                        let operator = program.[next+4]
                                                        doMaths value1 value2 operator, 6
                                                    // x = y
                                                    | false -> 
                                                        getValue program.[next+3] |> float, 4  
                                            let operator = program.[next+2]
                                            value1, value2, operator, steps
                                        | false ->
                                            let value1 = getValue program.[next+1] |> float
                                            let value2 = getValue program.[next+3] |> float
                                            let mathOperator = program.[next+2]
                                            let value1 = doMaths value1 value2 mathOperator 
                                            let value2, steps = 
                                                match List.contains program.[next+6] arithmeticOperators with
                                                    //  y + z = y + z
                                                    | true ->
                                                        let value1 = getValue program.[next+5] |> float
                                                        let value2 = getValue program.[next+7] |> float
                                                        let operator = program.[next+6]
                                                        doMaths value1 value2 operator, 8 
                                                    // y + z = x
                                                    | false -> 
                                                        getValue program.[next+5] |> float, 6
                                            let operator = program.[next+4]   
                                            value1, value2, operator, steps
                                let comparisonResult = condition item1 operator item2
                                let next' =
                                    match comparisonResult with
                                        | true -> next + steps
                                        | false -> 
                                            // Find the beginning of the else block
                                            let rec findElseBlock (program: string[]) next = 
                                                match program.[next] with
                                                    | "else" -> next+1   
                                                    | _ -> findElseBlock program (next+1)
                                            let endElseBlockPosition = findElseBlock program next
                                            endElseBlockPosition
                                next'
                            | "else" ->
                                let rec findEndIf (program: string[]) next = 
                                    match program.[next] with
                                        | "endif" -> next+1
                                        | _ -> findEndIf program (next+1)
                                let next' = findEndIf program next
                                next'
                            | "endif" -> next+1
                            // -- Loop
                            | "while" ->
                                let item1 = getValue program.[next+1] |> float
                                let item2 = getValue program.[next+3] |> float
                                let operator = program.[next+2]
                                let comparisonResult = condition item1 operator item2
                                let next' =
                                    match comparisonResult with
                                        | true -> next + 4
                                        | false -> 
                                            // Loop is done, go to the next step after the loop
                                            let rec findEndOfWhilePosition (program: string[]) next whileCount = 
                                                match program.[next], whileCount with
                                                    | "endWhile", 0 -> next + 1  
                                                    | "endWhile", _ -> findEndOfWhilePosition program (next+1) (whileCount-1)
                                                    | "while", _ -> findEndOfWhilePosition program (next+1) (whileCount+1)
                                                    | _, _ -> findEndOfWhilePosition program (next+1) whileCount
                                            let endWhilePosition = findEndOfWhilePosition program next -1
                                            endWhilePosition
                                next'
                            | "endWhile" ->
                                // If at the end of a loop go back to see if the loop is finished
                                let rec findWhilePosition (program: string[]) next whileCount = 
                                    match program.[next], whileCount with
                                        | "while", 0 -> next
                                        | "while", _ -> findWhilePosition program (next-1) (whileCount-1)
                                        | "endWhile", _ ->findWhilePosition program (next-1) (whileCount+1)
                                        | _, _ -> findWhilePosition program (next-1) whileCount
                                let next' = findWhilePosition program next -1             
                                next'
                            | _ ->
                                let next' =  
                                    match List.contains program.[next+1] modifyingOperators with
                                        // Change variable data                 
                                        | true ->
                                            let originalValue = getValue program.[next] |> float
                                            let inputValue = getValue program.[next+2] |> float 
                                            let operator = program.[next+1].[0] |> string
                                            let modifiedValue = doMaths originalValue inputValue operator
                                            let key = dictionary.Item program.[next]
                                            stack.[key] <- modifiedValue
                                            next+3
                                        | false ->
                                            printfn "\nError: Unknow action based on the word: %s in dictionary" program.[next] 
                                            next+1
                                next'
                    // Go to the next step in the program
                    parser program next'