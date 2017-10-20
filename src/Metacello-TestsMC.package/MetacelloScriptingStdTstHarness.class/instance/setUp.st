running
setUp
  super setUp.
  MetacelloPlatform current clearPackageCache.
  registry := MetacelloProjectRegistration registry.
  self setUpRepositories.
  self setUpRepositoryContents.
  MetacelloProjectRegistration resetRegistry.
  initialWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  self disableUndefinedSymbolTracking
    ifTrue: [ undefinedSymbols := MetacelloPlatform current disableUndefinedSybolUpdates ]