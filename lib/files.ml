(* 
  This defines the interaction between namespaces, statements, expressions and files.

  Considerations:
  - How do you split a project across multiple files?
  - How do you refer to external libraries?
  - How do you build a graph of dependencies without first parsing each file in isolation?
  - How do you refer to the standard library?
  = How do you incrementally parse, type and compile file systems?

  Elements:
  - `parse_file`
    - Declare all external names in a given file
      - Easy dependency graph / name resolution without having to type check
    - Produces a namespace by default
  - `parse_folder`
    - Ignore sub folders
    - Each file is a namespace
    - Big namespace for the whole thing
    - Config
      - Declares name
      - Entry point file / implicit
  - `parse_project`
    - Config file declares deps
      - By default `.modules`
      - Can point at other folder in config
    - External Dependencies
      - Exact Version in config
      - Hash in config.lock
      - Link in config.lock
      - Each package gets its own copy of everything
    - Internal Dependencies
      - Build dependency graph with file's externals and folder's names
      - No cyclic dependency
  
  Worfklow:
  - download and install single package
  - download and install all packages
  - compile folder
    - build namespace.json that represents the full project
  - compile project
    - build namespace.json that represents the full project
  - interpret file
*)

module File = struct
  module Name = struct
    include String
    module Set : Set.S with type elt = t = Set.Make(String)
    module Map : Map.S with type key = t = Map.Make(String)
  end

  type t = {
    external_names : Name.Set.t ;
    namespace : Types.nexpr ;
  }

  let parse ~(content : string) : t = ignore content ; assert false
end

module Folder = struct
  type entry_point = Default | Named of File.Name.t

  type t = {
    name : string ;
    entry_point : entry_point ;
    files : File.t File.Name.Map.t ;
  }

  let parse ~(file_path : string) : t = ignore file_path ; assert false
end



module Project = struct
  (* External Dependencies *)
  module ExtDep = struct
    module Name = struct
      include String
      module Map : Map.S with type key = t = Map.Make(String)
    end
    type t = {
      name : string ;
      version : string ;
    }
  end


  (* Internal Dependency Graph *)
  module IntDep = struct
    module FolderName = String
    module FolderPath = String

    module Graph = struct
      module G = Graph.Persistent.Graph.Concrete(FolderName)
      include G
    end
    module Map = Map.Make(FolderName)
  end

  module Export = struct
    type t = MainFolder of string
  end
  module Config = struct
    type t = {
      external_dependency_folder : string ;
      external_dependencies : ExtDep.t ExtDep.Name.Map.t ;
      export : Export.t ;
    }
  end

  type t = {
    config : Config.t ;
    idg : IntDep.Graph.t ;
    internal_dependecy_map : IntDep.FolderPath.t IntDep.Map.t ;
  }
end