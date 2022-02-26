module Tests

open Expecto
open FScan
open FScan.Expecto

[<Literal>]
let jsonFileData =
    @"{
    ""Animals"": [
        ""Aardvark"",
        ""Blue Whale"",
        ""Honey Badger"",
        ""Cuttlefish"",
        ""Stoat"",
        ""Hummingbird""
    ]
}
"

[<Tests>]
let tests =
    testList
        "samples"
        [ testCase "Directory traversal works"
          <| fun _ ->
              let rootPath = FilePath.bind "Root"
              Expect.isDirectory rootPath
              Expect.equal rootPath.Path "Root" "Root directory path was incorrect"
              let subdirectories = rootPath.GetSubdirectories()

              Expect.hasLength
                  subdirectories
                  1
                  "There should be exactly one subdirectory in the result set"

              let allSubdirectories = rootPath.GetSubdirectoriesRecursive()

              Expect.hasLength
                  allSubdirectories
                  2
                  "There should be exactly two subdirectories in the result set"

              let allChildren = rootPath.GetChildrenRecursive()

              Expect.hasLength
                  allChildren
                  6
                  "There should be exactly six children in the result set"



          testCase "File reading works"
          <| fun _ ->
              let fileToRead = FilePath.bind "Root/JsonFile.json"
              Expect.isFile fileToRead
              Expect.equal fileToRead.Path "Root/JsonFile.json" "Root directory path was incorrect"
              let content = FilePath.readAllText fileToRead
              Expect.isOk content "File could not be read"

              let content =
                  match content with
                  | Ok content -> content
                  | _ -> ""

              Expect.equal
                  (content.Trim())
                  (jsonFileData.Trim())
                  "Json file data was not read correctly"
          testCase "File writing works"
          <| fun _ ->
              let fileToWrite = FilePath.bind "Output/File.txt"
              Expect.isFile fileToWrite
              let written = fileToWrite.WriteAllText "Hello world!"
              Expect.isOk written "Failed to write file"
              let read = fileToWrite.ReadAllText()
              Expect.isOk read "File contents were unchanged"

              let content =
                  match read with
                  | Ok content -> content
                  | _ -> ""

              Expect.equal content "Hello world!" "File contents were incorrect"

          ]
