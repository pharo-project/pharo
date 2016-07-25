accessing
repositories
	^ (self useCache 
		ifTrue: [Array with: MCCacheRepository uniqueInstance] 
		ifFalse: [Array new]) , repositories select: #isValid