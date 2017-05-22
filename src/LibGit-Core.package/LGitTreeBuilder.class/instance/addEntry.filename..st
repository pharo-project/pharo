operations
addEntry: aLGitObject filename: aString
	self 
		addEntryWithId: aLGitObject id 
		filename: aString 
		filemode: aLGitObject class defaultFilemode