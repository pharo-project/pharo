accessing
prefixForOperation: aSymbol
	aSymbol == #insert ifTrue: [^ '+'].
	aSymbol == #remove ifTrue: [^ '-'].
	^ ' '