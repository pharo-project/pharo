accessing
resetWithStoreNamed: newName

	| fileReference |
	"First, ensure file is not already used."
	fileReference := OmFileStore 
		fileReferenceForStoreNamed: newName
		inDirectory: self directory.
	fileReference exists ifTrue: [
		FileExists signalWith: fileReference ].
	
	"Then, we can proceed."
	currentSession := Smalltalk session.
	currentImagePathString := self imagePathString.
	currentStore := 
		OmStoreFactory current
			named: newName
			inDirectory: self directory ensureCreateDirectory.
	currentStore writingDeferDuration: self writingDeferDuration.
	
	"Announce"
	self announcer announce: OmSessionStoreUpdated new.