accessing
resetWithStoreNamed: newName

	currentSession := Smalltalk session.
	currentImagePathString := self imagePathString.
	currentStore := 
		OmFileStore 
			newNamed: newName 
			inDirectory: self directory ensureCreateDirectory.
	self announcer announce: OmSessionStoreUpdated new.