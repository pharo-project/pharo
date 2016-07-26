instance creation
lookup: aStringOrSymbol

	^(SymbolTable like: aStringOrSymbol) ifNil: [
		NewSymbols like: aStringOrSymbol
	]