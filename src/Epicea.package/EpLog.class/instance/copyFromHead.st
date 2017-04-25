copying
copyFromHead
	^ self species newWithStore: 
		(OmFileStore
			named: store headReference globalName 
			inDirectory: store baseLocator asFileReference)