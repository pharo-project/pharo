accessing
answerDirectory
	"Set the receiver to answer a directory."
	
	self actionSelector: #selectedAnyFileDirectory.
	self fileSelectionBlock: self directoryFileSelectionBlock.
	self changed: #okEnabled