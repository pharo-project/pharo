public - file system operations
flushChangesFile
	
	flushChanges ifFalse: [ ^ self ].
	
	self changesFileStream ifNotNil: [:f | f flush ]