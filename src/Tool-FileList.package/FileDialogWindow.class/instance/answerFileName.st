actions
answerFileName
	"Set the receiver to answer the selected file name."
	
	self actionSelector: #selectedFileName.
	self changed: #okEnabled