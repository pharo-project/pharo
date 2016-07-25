actions
versionInfoForPackageDirectory: packageDirectory
    ^ ((MCReader readerClassForFileNamed: (self fileUtils directoryName: packageDirectory))
        on: (self fileUtils parentDirectoryOf: packageDirectory)
        fileName: (self fileUtils directoryName: packageDirectory))
        loadVersionInfo;
        info