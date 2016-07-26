encoding
encodeReferenceTo: anObject
	| index |
	index := substitutionIndexes
		at: anObject
		ifAbsent: [
			objectsIndexes
				at: anObject
				ifAbsent: [ FLObjectNotFound signalWith: anObject ] ].
	indexStream nextIndexPut: index