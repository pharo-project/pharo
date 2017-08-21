traits
removeTraitSelector: aSymbol
	[(self includesLocalSelector: aSymbol) not] assert.
	self basicRemoveSelector: aSymbol