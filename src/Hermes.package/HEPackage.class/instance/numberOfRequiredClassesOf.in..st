testing
numberOfRequiredClassesOf: aClass in: aCollection
	^ ((aClass allSuperclassesIn: aCollection) , ((self allSharedPoolsOf: aClass in: aCollection) flatCollect: [:e | {e} , (e allSuperclassesIn: aCollection)])) size