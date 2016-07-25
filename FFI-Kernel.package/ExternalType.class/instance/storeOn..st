printing
storeOn: aStream
	referentClass == nil
		ifTrue:[aStream nextPutAll: ExternalType name; space; nextPutAll: (AtomicTypeNames at: self atomicType)]
		ifFalse:[aStream nextPut: $(; nextPutAll: ExternalType name; space; nextPutAll: #structTypeNamed:; space;  store: referentClass name; nextPut: $)].
	self isPointerType ifTrue: [aStream space; nextPutAll: #asPointer].