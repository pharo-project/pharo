copying
copyFromHead
	^ self species newWithStore: 
		(OmFileStore
			named: headReference globalName 
			inDirectory: store baseLocator asFileReference)