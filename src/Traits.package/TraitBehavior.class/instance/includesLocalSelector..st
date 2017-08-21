testing method dictionary
includesLocalSelector: aSymbol
	^self basicLocalSelectors 
		ifNil: [self includesSelector: aSymbol]
		ifNotNil: [self localSelectors includes: aSymbol]