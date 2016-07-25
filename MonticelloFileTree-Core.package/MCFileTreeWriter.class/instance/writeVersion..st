visiting
writeVersion: aVersion
    | members |
    directory := (members := MCFileTreeRepository parseName: aVersion info name) last.
    self deleteExistingPackageStructureFor: members.
    self fileUtils ensureDirectoryExists: self packageFileDirectory.
    self initializePackageFileDirectoryCache.
    super writeVersion: aVersion