testing
okEnabled
	"Answer wether the ok button should be enabled."

	(#(selectedAnyFileDirectory selectedPathName)
			includes: self actionSelector) ifTrue: [^true].
	((#(saveSelectedFile saveForcedSelectedFile)
			includes: self actionSelector) and: [self fileNameText notEmpty]) ifTrue: [^true].
	(self actionSelector = #selectedFileName and: [
		self selectedFileName notNil]) ifTrue: [^true].
	^self selectedFileName notNil and: [self selectedFileEntry isDirectory not]