window menu
saveAsMenuAction
	"Save the receiver's contents string to a file, prompting the user for a file-name.  Suggest a reasonable file-name."

	| suggestedFileName |
	suggestedFileName := self askForFileNameToSave: self suggestedFileName.
	suggestedFileName ifNil: [ ^ self ].
	self fileName: suggestedFileName.
	self label: suggestedFileName.
	textModel ifNotNil: [ :model | self saveString: model getString toFile: suggestedFileName ]