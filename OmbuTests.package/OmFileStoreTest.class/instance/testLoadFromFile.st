tests
testLoadFromFile
	| loadedStore |
	store newEntry: (OmEntry content: 42).
	
	loadedStore := 
		(OmFileStore 
			named: store globalName 
			inDirectory: self directoryForTests)
			refresh;
			yourself.
	
	self 
		assert: (loadedStore entries collect: #content as: Array)
		equals: #(42)