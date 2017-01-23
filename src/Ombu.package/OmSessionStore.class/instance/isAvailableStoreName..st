testing
isAvailableStoreName: aName

	^ (OmFileStore 
		fileReferenceForStoreNamed: aName 
		inDirectory: self directory) exists not
