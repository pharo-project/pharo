filein/out
storeOn: aStream
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream
		cr; tab;
		nextPutAll: 'colorsFromArray: #('.
	self colors do: [:color |
		color storeArrayOn: aStream].
	aStream nextPutAll: ' ))'.