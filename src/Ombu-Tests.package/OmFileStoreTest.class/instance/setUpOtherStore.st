resources
setUpOtherStore

	^ OmFileStore 
		named: 'testingOtherStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory