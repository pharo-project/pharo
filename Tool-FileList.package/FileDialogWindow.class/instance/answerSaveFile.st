actions
answerSaveFile
	"Set the receiver to answer a new file stream."
	
	self actionSelector: #saveSelectedFile.
	self changed: #okEnabled