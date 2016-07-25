accessing
dragItemSelector: aSymbol
	dragItemSelector := aSymbol.
	aSymbol ifNotNil:[self dragEnabled: true].