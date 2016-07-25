running
setUpRepositoryContents: packageList sourceRepo: sourceRepo targetRepo: targetRepo
    packageList
        do: [ :package | 
            | versionInfo |
            versionInfo := sourceRepo versionInfoFromVersionNamed: package.
            targetRepo storeVersion: (sourceRepo versionWithInfo: versionInfo ifAbsent: [ self assert: false ]) ]