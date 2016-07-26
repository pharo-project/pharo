visiting
subPackageFileDirectoryFor: directoryNameOrPath
    ^ cachedPackageFileDirectoryEntries
        at: directoryNameOrPath
        ifAbsentPut: [ 
            | dir |
            dir := self fileUtils directoryFromPath: directoryNameOrPath relativeTo: self packageFileDirectory.
            self fileUtils ensureDirectoryExists: dir.
            dir ]