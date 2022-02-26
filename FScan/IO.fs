namespace FScan

[<AutoOpen>]
module IO =
    type FilePath =
        | File of File
        | Directory of Directory
        static member Bind(path: string) : FilePath =
            if System.IO.Directory.Exists path then
                Directory <| Directory.Bind path
            else
                File <| File.Bind path

        member self.isFile =
            match self with
            | File _ -> true
            | _ -> false

        member self.isDirectory =
            match self with
            | Directory _ -> true
            | _ -> false

        member self.Path =
            match self with
            | Directory directory -> directory.Path
            | File file -> file.Path

        member self.Name =
            match self with
            | Directory directory -> directory.Name
            | File file -> file.Name

        member self.FullPath() =
            match self with
            | Directory directory -> directory.FullPath()
            | File file -> file.FullPath()

        member self.WithFullPath() = FilePath.Bind <| self.FullPath()

        member self.PathRelativeTo(path: string) =
            match self with
            | Directory directory -> directory.PathRelativeTo path
            | File file -> file.PathRelativeTo path

        member self.RelativeToPath(path: string) =
            FilePath.Bind <| self.PathRelativeTo path

        member self.RelativeTo(other: FilePath) =
            FilePath.Bind <| self.PathRelativeTo other.Path

        member self.Parent() =
            match self with
            | Directory directory -> directory.Parent()
            | File file -> file.Parent()

        member self.Exists() =
            match self with
            | Directory directory -> directory.Exists()
            | File file -> file.Exists()

        member self.Create() =
            match self with
            | Directory directory -> directory.Create()
            | File file -> file.Create()

        member self.GetFiles() =
            match self with
            | Directory directory -> directory.GetFiles() |> Seq.map File
            | File _ -> Seq.empty

        member self.GetFilesRecursive() =
            match self with
            | Directory directory -> directory.GetFilesRecursive() |> Seq.map File
            | File _ -> Seq.empty

        member self.GetSubdirectory path =
            match self with
            | Directory directory ->
                directory.GetSubdirectory path
                |> Directory
                |> Some
            | File _ -> None

        member self.GetSubdirectories() =
            match self with
            | Directory directory -> directory.GetSubdirectories() |> Seq.map Directory
            | File _ -> Seq.empty

        member self.GetSubdirectoriesRecursive() =
            match self with
            | Directory directory ->
                directory.GetSubdirectoriesRecursive()
                |> Seq.map Directory
            | File _ -> Seq.empty

        member self.GetChildren() =
            Seq.append (self.GetSubdirectories()) (self.GetFiles())

        member self.GetChildrenRecursive() =
            Seq.append (self.GetSubdirectoriesRecursive()) (self.GetFilesRecursive())

        member self.ReadAllText() =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.ReadAllText()

        member self.ReadAllBytes() =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.ReadAllBytes()

        member self.ReadAllLines() =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.ReadAllLines()

        member self.WriteAllText content =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.WriteAllText content

        member self.WriteAllBytes content =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.WriteAllBytes content

        member self.WriteAllLines content =
            match self with
            | Directory _ -> Error "The given path refers to a directory"
            | File file -> file.WriteAllLines content

    and File =
        { Path: string
          Name: string
          Extension: string }
        static member Bind(path: string) : File =
            let name = System.IO.Path.GetFileName path
            let extension = System.IO.Path.GetExtension path

            { Path = path
              Name = name
              Extension = extension }

        member self.FullPath() = System.IO.Path.GetFullPath self.Path

        member self.PathRelativeTo path =
            System.IO.Path.GetRelativePath(path, self.Path)

        member self.Parent() =
            System.IO.Path.GetDirectoryName self.Path
            |> Directory.Bind

        member self.Exists() = System.IO.File.Exists self.Path

        member self.CreateParent() =
            let parent = self.Parent()

            if not <| parent.Exists() then
                parent.Create()

        member self.Create() =
            self.CreateParent()

            System.IO.File.Create(self.Path) |> ignore

        member self.ReadAllText() =
            try
                Ok <| System.IO.File.ReadAllText self.Path
            with
                | _ -> Error "The file could not be read"

        member self.ReadAllBytes() =
            try
                Ok <| System.IO.File.ReadAllBytes self.Path
            with
                | _ -> Error "The file could not be read"

        member self.ReadAllLines() =
            try
                Ok <| System.IO.File.ReadAllLines self.Path
            with
                | exn -> Error $"The file could not be written. {exn.Message}"


        member self.WriteAllText content =
            try

                self.CreateParent()

                Ok
                <| System.IO.File.WriteAllText(self.Path, content)
            with
                | exn -> Error $"The file could not be written. {exn.Message}"

        member self.WriteAllBytes content =
            try
                self.CreateParent()

                Ok
                <| System.IO.File.WriteAllBytes(self.Path, content)
            with
                | exn -> Error $"The file could not be written. {exn.Message}"


        member self.WriteAllLines content =
            try
                self.CreateParent()

                Ok
                <| System.IO.File.WriteAllLines(self.Path, content)
            with
                | _ -> Error "The file could not be written"

    and Directory =
        { Path: string
          Name: string }
        static member Bind(path: string) : Directory =
            let name = System.IO.Path.GetFileName path

            { Path = path; Name = name }

        member self.FullPath() = System.IO.Path.GetFullPath self.Path

        member self.PathRelativeTo path =
            System.IO.Path.GetRelativePath(path, self.Path)

        member self.Parent() =
            System.IO.Directory.GetParent(self.Path).Name
            |> Directory.Bind

        member self.Exists() = System.IO.File.Exists self.Path

        member self.Create() =
            System.IO.Directory.CreateDirectory self.Path
            |> ignore

        member self.GetFiles() =
            System.IO.Directory.EnumerateFiles(
                self.Path,
                "*",
                System.IO.SearchOption.TopDirectoryOnly
            )
            |> Seq.map File.Bind

        member self.GetFilesRecursive() =
            System.IO.Directory.EnumerateFiles(
                self.Path,
                "*",
                System.IO.SearchOption.AllDirectories
            )
            |> Seq.map File.Bind

        member self.GetSubdirectories() =
            System.IO.Directory.EnumerateDirectories(
                self.Path,
                "*",
                System.IO.SearchOption.TopDirectoryOnly
            )
            |> Seq.map Directory.Bind

        member self.GetSubdirectoriesRecursive() =
            System.IO.Directory.EnumerateDirectories(
                self.Path,
                "*",
                System.IO.SearchOption.AllDirectories
            )
            |> Seq.map Directory.Bind

        member self.GetSubdirectory(name: string) =
            { Path = System.IO.Path.Combine(self.Path, name)
              Name = name }

    module FilePath =
        let bind path = FilePath.Bind path
        let bindFile path = File <| File.Bind path
        let bindDirectory path = Directory <| Directory.Bind path
        let fromString = bind
        let isFile (entry: FilePath) = entry.isFile
        let isDirectory (entry: FilePath) = entry.isDirectory
        let path (entry: FilePath) = entry.Path
        let name (entry: FilePath) = entry.Name
        let fullPath (entry: FilePath) = entry.FullPath()
        let withFullPath (entry: FilePath) = entry.WithFullPath()
        let pathRelativeTo path (entry: FilePath) = entry.PathRelativeTo path
        let relativeToPath path (entry: FilePath) = entry.RelativeToPath path
        let relativeTo (other: FilePath) (entry: FilePath) = entry.RelativeTo other
        let files (entry: FilePath) = entry.GetFiles()
        let filesRecursive (entry: FilePath) = entry.GetFilesRecursive()
        let subdirectory name (entry: FilePath) = entry.GetSubdirectory name
        let subdirectories (entry: FilePath) = entry.GetSubdirectories()
        let subdirectoriesRecursive (entry: FilePath) = entry.GetSubdirectoriesRecursive()
        let parent (entry: FilePath) = entry.Parent()
        let exists (entry: FilePath) = entry.Exists()
        let create (entry: FilePath) = entry.Create()
        let children (entry: FilePath) = entry.GetChildren()
        let childrenRecursive (entry: FilePath) = entry.GetChildrenRecursive()
        let readAllText (entry: FilePath) = entry.ReadAllText()
        let readAllBytes (entry: FilePath) = entry.ReadAllBytes()
        let readAllLines (entry: FilePath) = entry.ReadAllLines()
        let writeAllText content (entry: FilePath) = entry.WriteAllText content
        let writeAllBytes content (entry: FilePath) = entry.WriteAllBytes content
        let writeAllLines content (entry: FilePath) = entry.WriteAllLines content
