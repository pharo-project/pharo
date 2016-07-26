initialization
allSymbolTablesDo: aBlock after: aSymbol

	NewSymbols do: aBlock after: aSymbol.
	SymbolTable do: aBlock after: aSymbol.