accessing
methodSourceFor: aSymbol
	^ (self changeClass includesSelector: aSymbol)
		ifTrue: [ self changeClass sourceCodeAt: aSymbol ]