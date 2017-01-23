accessing
fileReferenceForStoreNamed: aName inDirectory: baseDirectoryFileReference

	^ baseDirectoryFileReference / (aName, self defaultFileSuffix)