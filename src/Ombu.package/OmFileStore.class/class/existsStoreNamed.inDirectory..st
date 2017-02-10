testing
existsStoreNamed: aName inDirectory: aFileReference

	^ aFileReference exists and: [
		(self 
			fileReferenceForStoreNamed: aName 
			inDirectory: aFileReference) exists ]
