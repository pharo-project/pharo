actions
packageDescriptionsFromReadableFileNames
    ^ ((self readableFileNames collect: [ :fileName | self fileDirectoryOn: fileName ])
        select: [ :packageDirectory | self fileUtils directoryExists: packageDirectory ])
        collect: [ :packageDirectory | self packageDescriptionFromPackageDirectory: packageDirectory ]