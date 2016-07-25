writing
writeOn: stream
	"In most use cases, contentLength will have been asked first so that a representation is cached"
	
	representation isNil ifTrue: [ self computeRepresentation ].
	ZnUtils nextPutAll: representation on: stream