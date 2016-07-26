printing
storeOn: aStream 

	aStream nextPut: $#.
	(self isLiteralSymbol)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]