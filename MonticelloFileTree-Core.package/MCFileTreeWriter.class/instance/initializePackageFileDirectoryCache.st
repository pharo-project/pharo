visiting
initializePackageFileDirectoryCache
    cachedPackageFileDirectoryEntries := Dictionary new.
    self packageFileDirectory entries
        do: [ :entry | 
            entry isDirectory
                ifTrue: [ cachedPackageFileDirectoryEntries at: entry name put: (self fileUtils directoryFromEntry: entry) ] ]