initialization
newStore

	^ OmFileStore 
		named: 'tmp', UUID new asString36
		inDirectory: self directory ensureCreateDirectory