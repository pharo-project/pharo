encoding
encodeWeakReferenceTo: anObject

	indexStream 
		nextIndexPut: (objectsIndexes at: anObject ifAbsent: [objectsIndexes at: nil]) 
