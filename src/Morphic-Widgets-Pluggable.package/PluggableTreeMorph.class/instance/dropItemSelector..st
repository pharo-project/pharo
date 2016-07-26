accessing
dropItemSelector: aSymbol
	dropItemSelector := aSymbol.
	aSymbol ifNotNil:[self dropEnabled: true].