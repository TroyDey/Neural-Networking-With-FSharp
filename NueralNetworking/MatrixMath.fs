module MatrixMath

open System
open MatrixError
open Matrix

type MatrixMath() =
    member this.Add augend addend = raise (NotImplementedException "Add")
    member this.Subtract minuend subtrahend = raise (NotImplementedException "Subtract")
    member this.MultiplyScalar multiplicand (multiplier:double) = raise (NotImplementedException "MultiplyScalar")
    member this.Multiply multiplicand (multiplier:Matrix) = raise (NotImplementedException "Multiply")
    member this.Divide dividend divisor = raise (NotImplementedException "Divide")
    member this.DotProduct a b = raise (NotImplementedException "DotProduct")
    member this.Transpose matrix = raise (NotImplementedException "Transpose")
    member this.Identity size = raise (NotImplementedException "Identity")