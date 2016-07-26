public - file system operations
close
		
	| oldFiles oldQueue | 
	
	oldFiles := files.
	oldQueue := readOnlyQueue.
	files := Array new: 2.
	readOnlyQueue := SharedQueue new.

	self closeFileArray: oldFiles.

	oldQueue flushAllSuchThat: [ :each | self closeFileArray: each. true]