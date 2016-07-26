access
allSymbols
	"Answer all interned symbols"
	^Array streamContents:[:s|
		s nextPutAll: NewSymbols.
		s nextPutAll: OneCharacterSymbols.
		s nextPutAll: SymbolTable.
	].
