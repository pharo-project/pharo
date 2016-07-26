actions
answerFileEntry
	"Set the receiver to answer the selected file entry."
	
	self actionSelector: #selectedFileEntry.
	self changed: #okEnabled