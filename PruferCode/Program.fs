open System
open System.Linq

let other x y v =
    if v = x then
        y
    else
        x

let rec prufer(tab: (int * int)seq, sorted: (int)seq) = 
    if tab.Count() > 1 then
        let leaf = Seq.head sorted;
        let (x, y) = Seq.head tab
        if x <> leaf && y <> leaf then
            prufer( Seq.append (Seq.tail tab) [(x,y)], sorted)
        else
            let neighbour = other x y leaf
            neighbour :: prufer(Seq.tail tab, Seq.tail sorted)
    else
        []
    
let prufer_code(tab: (int * int)seq) =
    Console.WriteLine(tab)
    let newSorted = tab.Select(fun (x, _) -> x).Concat(tab.Select(fun (_, y) -> y)).Where(fun l -> tab.Count(fun (x, y) -> x = l || y = l) = 1).OrderBy(fun l -> l).ToList()
    Console.WriteLine(prufer(tab, newSorted))

[<EntryPoint>]
let main _ =
    prufer_code([(1, 2)])
    Console.WriteLine()

    prufer_code([(1, 2); (1, 3)])
    Console.WriteLine()

    prufer_code([(1, 2); (2, 3)])
    Console.WriteLine()

    prufer_code([(1, 3); (2, 3)])
    Console.WriteLine()

    prufer_code([(1, 4); (2, 4); (3, 4)])
    Console.WriteLine()
    0

