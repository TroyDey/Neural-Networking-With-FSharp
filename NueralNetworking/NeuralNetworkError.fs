module NeuralNetworkError

open System

type NeuralNetworkError (message:string, ?innerException:exn) =
    inherit Exception (message, 
        match innerException with | Some(ex) -> ex | _ -> null)
