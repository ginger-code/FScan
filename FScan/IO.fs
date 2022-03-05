[<AutoOpen>]
module FScan.IO

open FsToolkit.ErrorHandling
open FScan.Result

type FilePath =
    private
    | File of File
    | Directory of Directory
    static member Bind(path: string) : FilePath =
        if System.IO.Directory.Exists path then
            Directory <| Directory.Bind path
        else
            File <| File.Bind path

    static member BindFile(path: string) : FilePath = File <| File.Bind path
    static member BindDirectory(path: string) : FilePath = Directory <| Directory.Bind path

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
        result {
            return!
                match self with
                | Directory directory -> directory.FullPath()
                | File file -> file.FullPath()
        }

    member self.WithFullPath() =
        result {
            let! fullPath = self.FullPath()
            return FilePath.Bind <| fullPath
        }

    member self.PathRelativeTo(path: string) =
        result {
            return!
                match self with
                | Directory directory -> directory.PathRelativeTo path
                | File file -> file.PathRelativeTo path
        }

    member self.RelativeToPath(path: string) =
        result {
            let! path = self.PathRelativeTo path
            return FilePath.Bind path
        }

    member self.RelativeTo(other: FilePath) =
        result {
            let! path = self.PathRelativeTo other.Path
            return FilePath.Bind path
        }

    member self.Parent() =
        result {
            return!
                match self with
                | Directory directory -> directory.Parent()
                | File file -> file.Parent()
        }

    member self.Exists() =
        match self with
        | Directory directory -> directory.Exists()
        | File file -> file.Exists()

    member self.Create() =
        result {
            return!
                match self with
                | Directory directory -> directory.Create()
                | File file -> file.Create()
        }

    member self.CopyTo destination =
        match self with
        | Directory directory -> directory.CopyTo destination
        | File file -> file.CopyTo destination

    member self.MoveTo destination =
        match self with
        | Directory directory -> directory.MoveTo destination
        | File file -> file.MoveTo destination

    member self.GetFiles() =
        match self with
        | Directory directory ->
            result {
                let! files = directory.GetFiles()
                return files |> Seq.map File
            }
        | File _ -> Error <| exn "Files do not contain files"

    member self.GetFilesRecursive() =
        match self with
        | Directory directory ->
            result {
                let! files = directory.GetAllFiles()
                return files |> Seq.map File
            }
        | File _ -> Error <| exn "Files do not contain files"

    member self.GetSubdirectory path =
        match self with
        | Directory directory -> directory.GetSubdirectory path |> Directory |> Ok
        | File _ -> Error <| exn "Files do not contain subdirectories"

    member self.GetSubdirectories() =
        match self with
        | Directory directory ->
            result {
                let! subDirectories = directory.GetSubdirectories()
                return subDirectories |> Seq.map Directory
            }
        | File _ -> Error <| exn "Files do not contain subdirectories"

    member self.GetAllSubdirectories() =
        match self with
        | Directory directory ->
            result {
                let! subDirectories = directory.GetAllSubdirectories()
                return subDirectories |> Seq.map Directory
            }
        | File _ -> Error <| exn "Files do not contain subdirectories"

    member self.GetChildren() =
        result {
            let! dirs = self.GetSubdirectories()
            let! files = self.GetFiles()
            return Seq.append dirs files
        }

    member self.GetAllChildren() =
        result {
            let! dirs = self.GetAllSubdirectories()
            let! files = self.GetFilesRecursive()
            return Seq.append dirs files
        }

    member self.ReadAllText() =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
        | File file -> file.ReadAllText()

    member self.ReadAllBytes() =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
        | File file -> file.ReadAllBytes()

    member self.ReadAllLines() =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
        | File file -> file.ReadAllLines()

    member self.WriteAllText content =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
        | File file -> file.WriteAllText content

    member self.WriteAllBytes content =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
        | File file -> file.WriteAllBytes content

    member self.WriteAllLines content =
        match self with
        | Directory _ ->
            Error
            <| exn "The given path refers to a directory"
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

    member self.FullPath() =
        toResult
        <| fun _ -> System.IO.Path.GetFullPath self.Path

    member self.PathRelativeTo path =
        toResult
        <| fun _ -> System.IO.Path.GetRelativePath(path, self.Path)

    member self.Parent() =
        toResult
        <| fun _ ->
            System.IO.Path.GetDirectoryName self.Path
            |> Directory.Bind

    member self.Exists() = System.IO.File.Exists self.Path

    member self.CreateParent() =
        result {
            let! parent = self.Parent()
            let exists = parent.Exists()
            if not exists then do! parent.Create()
        }

    member self.Create() =
        result {
            do! self.CreateParent()
            do ignore <| System.IO.File.Create(self.Path)
        }

    member self.CopyTo destination =
        toResult
        <| fun _ ->
            match destination with
            | File destination -> System.IO.File.Copy(self.Path, destination.Path)
            | Directory _ -> failwith "Cannot copy file as a directory"

    member self.MoveTo destination =
        toResult
        <| fun _ ->
            match destination with
            | File destination -> System.IO.File.Move(self.Path, destination.Path)
            | Directory _ -> failwith "Cannot move file as a directory"

    member self.ReadAllText() =
        toResult
        <| fun _ -> System.IO.File.ReadAllText self.Path

    member self.ReadAllBytes() =
        toResult
        <| fun _ -> System.IO.File.ReadAllBytes self.Path

    member self.ReadAllLines() =
        toResult
        <| fun _ -> System.IO.File.ReadAllLines self.Path

    member self.WriteAllText content =
        result {
            do! self.CreateParent()
            do System.IO.File.WriteAllText(self.Path, content)

        }

    member self.WriteAllBytes content =
        result {
            do! self.CreateParent()
            do System.IO.File.WriteAllBytes(self.Path, content)

        }

    member self.WriteAllLines content =
        result {
            do! self.CreateParent()
            do System.IO.File.WriteAllLines(self.Path, content)
        }

and Directory =
    { Path: string
      Name: string }
    static member Bind(path: string) : Directory =
        let name = System.IO.Path.GetFileName path
        { Path = path; Name = name }

    member self.FullPath() =
        toResult
        <| fun _ -> System.IO.Path.GetFullPath self.Path

    member self.PathRelativeTo path =
        toResult
        <| fun _ -> System.IO.Path.GetRelativePath(path, self.Path)

    member self.Parent() =
        toResult
        <| fun _ ->
            System.IO.Directory.GetParent(self.Path).Name
            |> Directory.Bind

    member self.Exists() = System.IO.File.Exists self.Path

    member self.Create() =
        toResult
        <| fun _ ->
            System.IO.Directory.CreateDirectory self.Path
            |> ignore

    member self.CopyTo destination =
        let rec directoryCopy srcPath dstPath =
            if not <| System.IO.Directory.Exists(srcPath) then
                raise
                <| System.IO.DirectoryNotFoundException
                    $"Source directory does not exist or could not be found: {srcPath}"

            if not <| System.IO.Directory.Exists(dstPath) then
                System.IO.Directory.CreateDirectory(dstPath)
                |> ignore

            let srcDir = System.IO.DirectoryInfo(srcPath)

            for file in srcDir.GetFiles() do
                let fileDest =
                    System.IO.Path.Combine(dstPath, file.Name)

                file.CopyTo(fileDest, true) |> ignore

            for subDirectory in srcDir.GetDirectories() do
                let dstSubDir =
                    System.IO.Path.Combine(dstPath, subDirectory.Name)

                directoryCopy subDirectory.FullName dstSubDir

        toResult
        <| fun _ ->
            match destination with
            | Directory destination -> directoryCopy self.Path destination.Path
            | File _ -> failwith "Cannot copy directory as a file"

    member self.MoveTo destination =
        toResult
        <| fun _ ->
            match destination with
            | Directory destination -> System.IO.Directory.Move(self.Path, destination.Path)
            | File _ -> failwith "Cannot move directory as a file"

    member self.GetFiles() =
        toResult
        <| fun _ ->
            System.IO.Directory.EnumerateFiles(
                self.Path,
                "*",
                System.IO.SearchOption.TopDirectoryOnly
            )
            |> Seq.map File.Bind

    member self.GetAllFiles() =
        toResult
        <| fun _ ->
            System.IO.Directory.EnumerateFiles(
                self.Path,
                "*",
                System.IO.SearchOption.AllDirectories
            )
            |> Seq.map File.Bind

    member self.GetSubdirectories() =
        toResult
        <| fun _ ->
            System.IO.Directory.EnumerateDirectories(
                self.Path,
                "*",
                System.IO.SearchOption.TopDirectoryOnly
            )
            |> Seq.map Directory.Bind

    member self.GetAllSubdirectories() =
        toResult
        <| fun _ ->
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
    let bindFile path = FilePath.BindFile path

    let bindFileChecked path =
        let file = bindFile path

        if file.Exists() then
            Ok file
        else
            Error <| exn "File does not exist"

    let bindDirectory path = FilePath.BindDirectory path

    let bindDirectoryChecked path =
        let directory = bindDirectory path

        if directory.Exists() then
            Ok directory
        else
            Error <| exn "File does not exist"

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
    let allFiles (entry: FilePath) = entry.GetFilesRecursive()
    let subdirectory name (entry: FilePath) = entry.GetSubdirectory name
    let subdirectories (entry: FilePath) = entry.GetSubdirectories()
    let allSubdirectories (entry: FilePath) = entry.GetAllSubdirectories()
    let parent (entry: FilePath) = entry.Parent()
    let exists (entry: FilePath) = entry.Exists()
    let create (entry: FilePath) = entry.Create()
    let copyTo (source: FilePath) (destination: FilePath) = source.CopyTo(destination)
    let moveTo (source: FilePath) (destination: FilePath) = source.MoveTo(destination)
    let children (entry: FilePath) = entry.GetChildren()
    let allChildren (entry: FilePath) = entry.GetAllChildren()
    let readAllText (entry: FilePath) = entry.ReadAllText()
    let readAllBytes (entry: FilePath) = entry.ReadAllBytes()
    let readAllLines (entry: FilePath) = entry.ReadAllLines()
    let writeAllText content (entry: FilePath) = entry.WriteAllText content
    let writeAllBytes content (entry: FilePath) = entry.WriteAllBytes content
    let writeAllLines content (entry: FilePath) = entry.WriteAllLines content
