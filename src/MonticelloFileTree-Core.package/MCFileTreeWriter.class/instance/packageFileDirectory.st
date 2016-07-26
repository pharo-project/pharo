writing
packageFileDirectory
    packageFileDirectory
        ifNil: [ packageFileDirectory := self fileUtils directoryFromPath: directory relativeTo: stream directory ].
    ^ packageFileDirectory