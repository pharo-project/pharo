resources
setUpStore	

	^ OmFileStore 
		named: 'testingStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory