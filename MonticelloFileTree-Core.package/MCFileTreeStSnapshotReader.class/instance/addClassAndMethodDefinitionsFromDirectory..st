utilities
addClassAndMethodDefinitionsFromDirectory: aDirectory
    | snapshot classes entries extensions |
    snapshot := self fileUtils directoryFromPath: 'snapshot' relativeTo: aDirectory.
    classes := self fileUtils directoryFromPath: 'classes' relativeTo: snapshot.
    (self fileUtils directoryExists: classes)
        ifTrue: [ 
            self addClassAndMethodDefinitionsFromDirectoryEntries: (entries := classes entries).	"load .st files from subdirectories (*.class) of snapshot/classes ... class defintions"
            entries
                do: [ :classDirectoryEntry | 
                    | classDirectory |
                    classDirectory := self fileUtils directoryFromEntry: classDirectoryEntry.
                    self addClassAndMethodDefinitionsFromDirectoryEntries: classDirectory entries	"load .st files from subdirectories (class or instance) of snapshot/classes/*.class ... method definitions" ] ].
    extensions := self fileUtils directoryFromPath: 'extensions' relativeTo: snapshot.
    (self fileUtils directoryExists: extensions)
        ifTrue: [ 
            extensions entries
                do: [ :classDirectoryEntry | 
                    | classDirectory |
                    classDirectory := self fileUtils directoryFromEntry: classDirectoryEntry.
                    self addClassAndMethodDefinitionsFromDirectoryEntries: classDirectory entries	"load .st files from subdirectories (class or instance) of snapshot/extensions/*.class ... method definitions" ] ]