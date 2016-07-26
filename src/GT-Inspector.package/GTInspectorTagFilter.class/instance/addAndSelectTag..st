adding
addAndSelectTag: aSymbol

	self addTag: aSymbol.
	(selectedTags includes: aSymbol) ifFalse:  [selectedTags add: aSymbol]