instance creation
newFileNameTextEntry
	"Answer a new file name text entry morph."

	^self
		newAutoAcceptTextEntryFor: self
		getText: #fileNameText
		setText: #fileNameText:
		getEnabled: nil
		help: 'File name filter pattern' translated