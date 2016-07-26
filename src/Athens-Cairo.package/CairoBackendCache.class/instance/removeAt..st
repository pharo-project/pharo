removing
removeAt: anObject

	^ cache removeKey: anObject  ifAbsent: nil