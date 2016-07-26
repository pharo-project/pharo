reporting
reportSources: aStream
	self header: 'Image Sources' on: aStream.
	aStream nextPutAll: SourceFiles class printString; cr.
	aStream nextPutAll: SourceFiles reportString.