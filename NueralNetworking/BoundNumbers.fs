module BoundNumbers

open System

let Smallest = -1.0E20
let Largest = 1.0E20

let Bound d =
    if d < Smallest then
        Smallest
    elif d > Largest then
        Largest
    else 
        d

let Exp d =
    Bound(Math.Exp(d))