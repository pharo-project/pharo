tests
testEnsureDeleteFile
	| loadedStore |
	store newEntry: (OmEntry content: 42).
	store flush.

	store ensureDeleteFile.
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			refresh;
			yourself.
	
	self assert: loadedStore entriesCount equals: 0.
