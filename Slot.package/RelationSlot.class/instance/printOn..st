printing
printOn: aStream
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name.
	self hasInverse 
		ifTrue: [ 
			aStream
				nextPutAll: ' inverse: ';
				store: inverseName ].
	aStream
		nextPutAll: ' inClass: ';
		store: self targetClassName.
