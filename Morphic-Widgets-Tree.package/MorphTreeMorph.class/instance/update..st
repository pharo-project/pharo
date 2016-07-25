updating
update: aSymbol
	aSymbol = self nodeListSelector
		ifTrue: [ ^ self updateList ].
	super update: aSymbol 