testing
numberOfRequiredClassesOf: aClass in: aCollection
	| requiredClasses |
	
	requiredClasses := aClass allSuperclasses , ((self allSharedPoolsOf: aClass) flatCollect: #withAllSuperclasses).
	^ (aCollection select: [:e | requiredClasses includes: e]) size