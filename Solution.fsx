//  8. Нахождение минимального произведения соседних элементов списка


//Method 1: Library Functions
let minProductOfNeighbours1 (lst: float list) =
    match lst with
    | [] | [_] -> None // если список пустой или содержит только один элемент, возвращаем None
    | _ ->
        lst
        |> List.pairwise // создаем пары соседних элементов
        |> List.map (fun (x, y) -> x * y) // вычисляем произведение каждой пары
        |> List.min // находим минимальное произведение
        |> Some // возвращаем минимальное произведение, обернутое в тип 'a option


// Method 2: Recursion
let rec minProductOfNeighbours2_0 (lst: float list) flag min_prod =
    match lst with
    | [] | [_] ->
        if flag = 1 then
            None
        else
            Some(min_prod)
    | x::y::t ->
        let prod = x * y
        if prod < min_prod || flag = 1 then
            minProductOfNeighbours2_0 (y::t) 0 prod
        else
            minProductOfNeighbours2_0 (y::t) 0 min_prod

let rec minProductOfNeighbours2 (lst: float list) =
    let rec loop lst minProd =
        match lst with
        | [] | [_] -> Some minProd
        | x::y::t ->
            let prod = x * y
            if prod < minProd then
                loop (y::t) prod
            else
                loop (y::t) minProd
    
    match lst with
    | [] | [_] -> None
    | x::y::t -> loop (y::t) (x * y)


// Method 3: Tail Recursion
let rec tailRec lst minProd =
    match lst with
    | [] | [_] -> Some minProd
    | x::y::t ->
        if (x * y) < minProd then
            tailRec (y::t) (x * y)
        else
            tailRec (y::t) minProd
let rec minProductOfNeighbours3 (lst: float list) =  
    match lst with
    | [] | [_] -> None
    | x::y::t -> tailRec (y::t) (x * y)




let numbers = [3.; 5.; -2.; 9.; 7.]
(*
[]
[1.]
[1.; 2.]
[1.; 2.; 3.]
[1.; 2.; -3.; -4.]
[0.; 1.; 2.; 3.; 4.; -5.]
[0.; 1.; 2.; 3.; 4.; 5.]
*)

match minProductOfNeighbours1 numbers with
| Some minProduct -> printfn "Минимальное произведение соседних элементов: %f" minProduct
| None -> printfn "Список слишком мал"

match minProductOfNeighbours2_0 numbers 1 0 with
| Some(min_prod) -> printfn "Минимальное произведение соседних элементов: %f" min_prod
| None -> printfn "Список слишком мал."

match minProductOfNeighbours2 numbers with
| Some(min_prod) -> printfn "Минимальное произведение соседних элементов: %f" min_prod
| None -> printfn "Список слишком мал."

match minProductOfNeighbours3 numbers with
| Some(min_prod) -> printfn "Минимальное произведение соседних элементов: %f" min_prod
| None -> printfn "Список слишком мал."

