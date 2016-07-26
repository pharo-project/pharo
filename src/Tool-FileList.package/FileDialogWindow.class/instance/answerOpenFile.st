actions
answerOpenFile
	"Set the receiver to answer a new file stream on an existing file."
	
	self actionSelector: #openSelectedFile.
	self changed: #okEnabled