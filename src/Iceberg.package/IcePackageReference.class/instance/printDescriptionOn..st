printing
printDescriptionOn: aStream
	aStream nextPut: $(;
		nextPutAll: packageName;
		nextPutAll: ', ';
		nextPutAll: (self isLoaded ifTrue: 'loaded' ifFalse: 'not loaded');
		nextPut: $)
		