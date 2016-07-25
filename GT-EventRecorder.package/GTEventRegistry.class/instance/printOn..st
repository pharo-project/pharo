printing
printOn: aStream
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: (self isLiberated ifTrue: [ 'liberated' ] ifFalse: [ 'used' ]);
		nextPut: $)