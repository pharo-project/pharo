private
directoryForDirectoryNamed: directoryNameOrPath
    ^ directoryNameOrPath = '.'
        ifTrue: [ stream packageFileDirectory ]
        ifFalse: [ stream subPackageFileDirectoryFor: directoryNameOrPath ]