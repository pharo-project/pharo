initialization
setUpStore	

	^ OmBlockFileStore
		named: 'testingStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory