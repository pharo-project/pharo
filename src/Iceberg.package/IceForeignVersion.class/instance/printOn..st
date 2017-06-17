printing
printOn: aStream
	aStream print: self class;
		nextPut: $(;
		nextPutAll: 
			(mcVersion 
				ifNotNil: [ mcVersion printString ] 
				ifNil: [ self package name ]);
		nextPut: $).
