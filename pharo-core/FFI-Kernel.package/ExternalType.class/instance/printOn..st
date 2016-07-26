printing
printOn: aStream
	referentClass == nil
		ifTrue:[aStream nextPutAll: (AtomicTypeNames at: self atomicType)]
		ifFalse:[aStream nextPutAll: referentClass name].
	self isPointerType ifTrue:[aStream nextPut: $*].