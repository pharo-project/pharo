testing
includesSelector: aSymbol in: aClass 
	^(environment includesSelector: aSymbol in: aClass) 
		and: [(self selectorCacheFor: aClass) includes: aSymbol]