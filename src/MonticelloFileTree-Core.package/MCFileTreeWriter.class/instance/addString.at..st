writing
addString: string at: fileNameOrPath
    "fileNameOrPath may have one or two elements"

    | utils fullPath path |
    utils := MCFileTreeFileUtils current.
    path := MCFileTreePackageStructureStWriter useCypressWriter
        ifTrue: [ 
            fullPath := utils
                buildPathFrom:
                    {(MCFileTreeStCypressWriter monticelloMetaDirName).
                    fileNameOrPath}.
            utils ensureFilePathExists: fullPath relativeTo: self packageFileDirectory.
            fullPath ]
        ifFalse: [ fileNameOrPath ].
    string isEmpty
        ifFalse: [ utils writeStreamFor: path in: self packageFileDirectory do: [ :file | file nextPutAll: string ] ]