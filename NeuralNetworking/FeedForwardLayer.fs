module FeedForwardNeuralNet

open System
open MatrixError
open Matrix
open MatrixMath
open ActivationFunctions

type FeedForwardLayer(activationFunction:IActivationFunction, neuronCount) =
    let mutable weightMatrix:Option<Matrix> = None
    let mutable nextLayer:Option<FeedForwardLayer> = None
    let mutable previousLayer:Option<FeedForwardLayer> = None
    let mutable output:Option<double[]> = None

    new(neuronCount) = FeedForwardLayer((new SigmoidActivation()), neuronCount)

    member this.WeightMatrix
        with get() = weightMatrix

    member this.NeuronCount
        with get() = neuronCount

    member this.NextLayer
        with get() = nextLayer

    member this.PreviousLayer
        with get() = previousLayer

    member this.Output
        with get() = output

    member this.ActivationFunction
        with get() = activationFunction

    member this.IsHidden
        with get() = 
            match (nextLayer, previousLayer) with
            | (None, None) -> true
            | _ -> false            

    member this.IsInput
        with get() = 
            match (nextLayer, previousLayer) with
            | (Some(_), None) -> true
            | _ -> false        

    member this.IsOutput
        with get() = 
            match (nextLayer, previousLayer) with
            | (None, Some(_)) -> true
            | _ -> false        

    member this.ComputeOutput (pattern:double[]) =
        if pattern <> null then
            Array.mapi (fun i e -> this.SetOutputAtIndex i e) pattern |> ignore
        
        let inputMatrix = this.CreateInputMatrix(this.Output.Value)        

        for i = 0 to nextLayer.Value.NeuronCount do
            let col = weightMatrix.Value.GetCol i
            let sum = MatrixMath.DotProduct col inputMatrix

            this.NextLayer.Value.SetOutputAtIndex i (this.ActivationFunction.ActivationFunction sum)
            
        output

    member this.CreateInputMatrix (pattern:double[]) =
        let result = new Matrix(1, pattern.Length + 1)

        for i = 0 to pattern.Length do
            result.[0, i] <- pattern.[i]

        // add a "fake" first column to the input so that the threshold is
        // always multiplied by one, resulting in it just being added.
        result.[0, pattern.Length] <- 1.0

        result

    member this.GetOutputAtIndex index = 
        match output with
        | Some(out) -> out.[index]
        | None -> 0.0

    member this.SetOutputAtIndex index value = 
        match output with
        | Some(out) -> out.[index] <- value
        | None -> ()

    member this.Reset () = 
        match weightMatrix with
        | Some(matrix) ->  matrix.Randomize -1.0 1.0
        | None -> ()

    member this.Prune () = raise (NotImplementedException "")
    