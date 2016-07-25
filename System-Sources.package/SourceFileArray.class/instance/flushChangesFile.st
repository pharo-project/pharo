public - file system operations
flushChangesFile
	
	self changesFileStream ifNotNil: [:f | f flush ]