accessing
changeClassName: aSymbol
	className := aSymbol.
	isMeta isNil
		ifTrue: [ isMeta := false ]