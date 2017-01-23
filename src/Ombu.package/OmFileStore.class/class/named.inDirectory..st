instance creation
named: aName inDirectory: baseDirectoryFileReference

	^ self
		named: aName
		inFile: (self fileReferenceForStoreNamed: aName inDirectory: baseDirectoryFileReference)