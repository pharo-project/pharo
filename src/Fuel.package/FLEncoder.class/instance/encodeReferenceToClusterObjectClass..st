encoding
encodeReferenceToClusterObjectClass: aClass 
	indexStream nextIndexPut: (objectsIndexes
		at: aClass
		ifAbsent: [ FLObjectNotFound signalWith: aClass ])