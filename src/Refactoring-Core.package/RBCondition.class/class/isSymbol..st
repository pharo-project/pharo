instance creation
isSymbol: aString
	^self new
		type: (Array with: #isSymbol with: aString)
		block: [aString isSymbol]
		errorString: aString , ' is <1?:not >a symbol'