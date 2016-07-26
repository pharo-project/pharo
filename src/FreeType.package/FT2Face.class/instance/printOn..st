printing
printOn: aStream 
	super printOn: aStream.
	handle ifNil: [^self].
	"self familyName isNil ifTrue: [ self loadFields ]."
	aStream
		nextPut: $[;
		nextPutAll: (self familyName ifNil: ['?']);
		space;
		nextPutAll: (self styleName ifNil: ['?']);
		nextPut: $]