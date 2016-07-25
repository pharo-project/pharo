accessing
versionFrom: aVersionReferenceString
    "until we no longer find .tree directories in the wild"

    (self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
        select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ]
        thenCollect: [ :packageDirectory | 
            (self versionInfoForPackageDirectory: packageDirectory) name = aVersionReferenceString
                ifTrue: [ ^ self loadVersionFromFileNamed: (self fileUtils directoryName: packageDirectory) ] ].
    ^ nil