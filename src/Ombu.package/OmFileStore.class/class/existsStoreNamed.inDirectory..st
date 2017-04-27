testing
existsStoreNamed: aGlobalName inDirectory: aFileReference

	^ aFileReference exists and: [
		(self 
			fileReferenceForStoreNamed: aGlobalName 
			inDirectory: aFileReference) exists ]
