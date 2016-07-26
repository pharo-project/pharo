updating
update: aParameter
	"Receive a change notice from an object of whom the receiver is a dependent"

	(aParameter == #fileListChanged) ifTrue: [self updateFileList].
	super update: aParameter