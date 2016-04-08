namespace NeuralNet.Training.Backpropagation

open Matrix
open NeuralNet.FeedForward

type BackpropagationLayer(currentLayer:FeedForwardLayer, nextLayer:Option<BackpropagationLayer>) =
    let neuronCount = currentLayer.NeuronCount
    let error:double[] = Array.zeroCreate neuronCount
    let errorDelta = Array.zeroCreate neuronCount
    let mutable accMatrixDelta:Option<Matrix> = None
    let mutable matrixDelta = None
    let mutable biasRow:Option<int> = None

    let init() =
        if Option.isSome nextLayer then
            accMatrixDelta <- Some (new Matrix(neuronCount + 1, currentLayer.NextLayer.Value.NeuronCount))
            matrixDelta <- Some (new Matrix(neuronCount + 1, currentLayer.NextLayer.Value.NeuronCount))
            biasRow <- Some neuronCount

    do init()

    member this.Error
        with get(idx) = error.[idx]
        and set(idx) (value:double) = error.[idx] <- value

    member this.ErrorDelta
        with get(idx) = errorDelta.[idx]
        and set(idx) (value:double) = errorDelta.[idx] <- value

    member this.AccumulateMatrixDelta row col value =
        accMatrixDelta.Value.Add(row, col, value)

    member this.AccumulateThresholdDelta col value =
        accMatrixDelta.Value.Add(biasRow.Value, col, value)

    member this.CalculateError () =
        for i = 0 to currentLayer.NextLayer.Value.NeuronCount do
            for j = 0 to neuronCount do
                this.AccumulateMatrixDelta j i (nextLayer.Value.ErrorDelta(i) * currentLayer.GetOutputAtIndex(j)) |> ignore
                this.Error(j) <- this.Error(j) + currentLayer.WeightMatrix.Value.[j, i] * nextLayer.Value.ErrorDelta(i)
            (this.AccumulateThresholdDelta i (nextLayer.Value.ErrorDelta(i))) |> ignore

        if currentLayer.IsHidden then
            for i = 0 to neuronCount do
                this.ErrorDelta(i) <- BoundNumbers.Bound(this.CalculateDelta(i))


    member this.CalculateError (ideal:double[]) =
        for i = 0 to currentLayer.NeuronCount do
            this.Error(i) <- ideal.[i] - currentLayer.GetOutputAtIndex(i)
            this.ErrorDelta(i) <- BoundNumbers.Bound(this.CalculateDelta i)

    member this.CalculateDelta idx = 
        (this.Error idx) * currentLayer.ActivationFunction.DerivativeFunction(currentLayer.GetOutputAtIndex(idx))

    member this.ClearError () =
        Array.fill error 0 error.Length 0.0

    member this.Learn learnRate momentum =
        if Option.isSome currentLayer.WeightMatrix then
            let m1 = MatrixMath.MultiplyByScalar accMatrixDelta.Value learnRate
            let m2 = MatrixMath.MultiplyByScalar matrixDelta.Value momentum
            matrixDelta <- Some(MatrixMath.Add(m1, m2))
            currentLayer.WeightMatrix <- Some((MatrixMath.Add(currentLayer.WeightMatrix.Value, matrixDelta.Value)))
            accMatrixDelta.Value.Clear()