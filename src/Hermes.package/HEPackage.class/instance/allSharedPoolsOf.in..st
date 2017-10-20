testing
allSharedPoolsOf: aClass in: aCollection
	^ ({aClass} , (aClass allSuperclassesIn: aCollection)) flatCollect: [:e | e sharedPoolsIn: aCollection].