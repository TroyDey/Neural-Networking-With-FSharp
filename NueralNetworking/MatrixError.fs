module MatrixError

open System

type MatrixError (message:string, ?innerException:exn) =
    inherit Exception (message, 
        match innerException with | Some(ex) -> ex | _ -> null)