modifying
addEntryNamed: name withContents: aString
	self builder 
		addEntry: (LGitBlob of: self repository fromString: aString)
		filename: name.
		
	self changes add: name.
	hasChanges := true