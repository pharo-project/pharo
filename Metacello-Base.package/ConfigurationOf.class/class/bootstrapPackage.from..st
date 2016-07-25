private
bootstrapPackage: aString from: aPath
    | repository version |
    repository := (MCCacheRepository default includesVersionNamed: aString)
        ifTrue: [ MCCacheRepository default ]
        ifFalse: [ MCHttpRepository location: aPath user: '' password: '' ].
    self
        retry: [ 
            repository
                versionReaderForFileNamed: aString , '.mcz'
                do: [ :reader | 
                    version := reader version.
                    version load.
                    version workingCopy repositoryGroup addRepository: repository ] ]