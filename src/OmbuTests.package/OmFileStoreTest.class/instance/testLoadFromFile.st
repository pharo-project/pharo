tests
testLoadFromFile
	| loadedStore |
	store newEntry: (OmEntry content: 42).
	store flush.
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			refresh;
			yourself.
	
	self 
		assert: (loadedStore entries collect: #content as: Array)
		equals: #(42)