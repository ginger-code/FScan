module FScan.Result 
    let internal toResult f =
        try
            Ok <| f ()
        with
            | ex -> Error ex
