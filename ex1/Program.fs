open System

// Тип бинарного дерева
type Tree =
    | Null
    | Head of int * Tree * Tree

// Функция вставки числа в дерево
let rec insert newValue tree =
    match tree with
    | Null -> Head(newValue, Null, Null) // Если пустое место — создать узел
    | Head(value, left, right) ->
        if newValue < value then
            // Если новое число меньше текущего — рекурсивно уходит налево
            Head(value, insert newValue left, right)
        else
            // Если больше или равно — рекурсивно уходит направо
            Head(value, left, insert newValue right)

// Функция генерации случайных чисел
let generateRandom count =
    let rnd = Random()

    let rec loop currentCount accTree = 
        if currentCount = 0 then
            accTree
        else
            let randomNumber = rnd.Next(1, 101)
            loop(currentCount - 1)(insert randomNumber accTree)
    
    loop count Null

// Функция map для дерева чисел
let rec treeMap f tree =
    match tree with
    | Null -> Null
    | Head(value, left, right) -> 
        Head(f value, treeMap f left, treeMap f right)

// Перевод числа в строку и дублирует её превращая в int
let duplicateNumber (n: int) = 
    let s = string n
    int (s + s)

[<EntryPoint>]
let main _ =
    printf "Введите количество элементов в дереве: "
    let count = int (Console.ReadLine())
    
    // Объявление дерева
    let myTree = generateRandom count
    printfn "\nИсходное дерево: %A" myTree

    // Применение treeMap для дубллирования
    let newTree = treeMap duplicateNumber myTree
    printfn "\nНовое дерево: %A" newTree
    
    0
