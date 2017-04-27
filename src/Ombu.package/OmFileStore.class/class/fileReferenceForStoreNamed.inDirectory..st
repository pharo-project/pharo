accessing
fileReferenceForStoreNamed: aGlobalName inDirectory: baseDirectoryFileReference

	^ baseDirectoryFileReference / (aGlobalName, self defaultFileSuffix)