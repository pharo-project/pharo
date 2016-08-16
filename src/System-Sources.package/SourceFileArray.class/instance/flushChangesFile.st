public - file system operations
flushChangesFile
	
	flushChanges == false ifTrue: [ ^ self ].
	
	self changesFileStream ifNotNil: [:f | f flush ]