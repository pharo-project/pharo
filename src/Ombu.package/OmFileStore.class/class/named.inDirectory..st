instance creation
named: aGlobalName inDirectory: baseDirectoryFileReference

	^ self
		named: aGlobalName
		inFile: (self fileReferenceForStoreNamed: aGlobalName inDirectory: baseDirectoryFileReference)