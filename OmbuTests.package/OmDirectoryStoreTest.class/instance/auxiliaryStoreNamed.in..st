resources
auxiliaryStoreNamed: globalName in: aDirectoryFileReference

	| auxiliaryStore |
	auxiliaryStore := OmFileStore 
		named: globalName 
		inDirectory: aDirectoryFileReference.
		
	auxiliaryStore 
		newEntry: (OmEntry content: 42);
		newEntry: (OmEntry content: 43).