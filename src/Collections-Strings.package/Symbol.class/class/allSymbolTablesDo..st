initialization
allSymbolTablesDo: aBlock

	NewSymbols do: aBlock.
	SymbolTable do: aBlock.