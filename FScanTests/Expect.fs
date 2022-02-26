module FScan.Expecto

module Expect =

    open Expecto
    open FScan

    let isDirectory (path: FilePath) =
        Expect.isTrue path.isDirectory "Path is not a directory"

    let isFile (path: FilePath) =
        Expect.isTrue path.isFile "Path is not a file"
