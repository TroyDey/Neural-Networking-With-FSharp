module ActivationFunctions

open System
open NeuralNet.FeedForward

type IActivationFunction =
    abstract member ActivationFunction : double -> double
    abstract member DerivativeFunction : double -> double

type LinearActivation() =
    interface IActivationFunction with
        member this.ActivationFunction input = input
        member this.DerivativeFunction input = raise (NeuralNetworkError "Can't use the linear activation function where a derivative is required.")

type SigmoidActivation() =
    interface IActivationFunction with
        member this.ActivationFunction input = 1.0 / (1.0 + BoundNumbers.Exp(-1.0 * input))
        member this.DerivativeFunction input = input * (1.0 - input)

type HyperbolicTangentActivation() =
    let tanh input = (BoundNumbers.Exp(input * 2.0) - 1.0) /(BoundNumbers.Exp(input * 2.0) + 1.0)
    interface IActivationFunction with 
        member this.ActivationFunction input = tanh input
        member this.DerivativeFunction input = (1.0 - Math.Pow((tanh input), 2.0))