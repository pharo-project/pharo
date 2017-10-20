querying
allSuperclassesIn: aCollection
	^ aCollection detect:[:aSuper | aSuper className = self superclass]
		ifFound:[ :miSuper | { miSuper } , (miSuper allSuperclassesIn: aCollection) ]
		ifNone:[ #() ]