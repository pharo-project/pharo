accessing
externalTypeWithArity
	^ self pointerArity > 0 
		ifTrue: [ self externalType asPointerType ]
		ifFalse: [ self externalType ]