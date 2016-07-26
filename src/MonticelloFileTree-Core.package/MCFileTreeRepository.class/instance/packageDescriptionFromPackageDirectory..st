actions
packageDescriptionFromPackageDirectory: packageDirectory
    | filename info extension |
    filename := self fileUtils current directoryName: packageDirectory.
    extension := filename copyFrom: (filename lastIndexOf: $.) to: filename size.
    ^ ((self packageExtension ~= '.package'
        and: [ 
            (self fileUtils filePathExists: 'version' relativeTo: packageDirectory)
                and: [ self fileUtils filePathExists: 'package' relativeTo: packageDirectory ] ])
        or: [ 
            | dir |
            dir := self fileUtils
                directoryFromPath: MCFileTreeStCypressWriter monticelloMetaDirName
                relativeTo: packageDirectory.
            self fileUtils directoryExists: dir ])
        ifTrue: [ 
            info := self versionInfoForPackageDirectory: packageDirectory.
            self parseName: info name extension: extension ]
        ifFalse: [ 
            {(filename copyFrom: 1 to: (filename lastIndexOf: $.) - 1).
            'cypress'.
            1.
            filename} ]