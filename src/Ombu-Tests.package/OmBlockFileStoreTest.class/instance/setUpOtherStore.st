initialization
setUpOtherStore

	^ OmBlockFileStore
		named: 'testingOtherStore', UUID new asString36
		inDirectory: self directoryForTests ensureCreateDirectory