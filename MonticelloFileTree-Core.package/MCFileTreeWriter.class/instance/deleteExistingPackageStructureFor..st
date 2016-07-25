visiting
deleteExistingPackageStructureFor: members
    "destroy .tree and .pkg directory structure .. otherwise it won't be pretty"

    | alternateDirName alternateDir packageDirectoryName |
    (self fileUtils directoryExists: self packageFileDirectory)
        ifTrue: [ self fileUtils deleteAll: self packageFileDirectory ].
    packageDirectoryName := self fileUtils directoryName: self packageFileDirectory.
    alternateDirName := (packageDirectoryName endsWith: '.pkg')
        ifTrue: [ members first , '.tree' ]
        ifFalse: [ 
            (packageDirectoryName endsWith: '.tree')
                ifTrue: [ members first , '.pkg' ]
                ifFalse: [ 
                    (packageDirectoryName endsWith: '.package')
                        ifTrue: [ members first , '.pkg' ] ] ].
    alternateDirName
        ifNotNil: [ 
            alternateDir := self fileUtils directoryFromPath: alternateDirName relativeTo: stream directory.
            (self fileUtils directoryExists: alternateDir)
                ifTrue: [ self fileUtils deleteAll: alternateDir ] ]