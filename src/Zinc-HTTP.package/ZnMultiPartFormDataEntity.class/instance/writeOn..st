writing
writeOn: stream
	"We always go via our representation because we need a bivalent stream"
	
	representation isNil ifTrue: [ self computeRepresentation ].
	ZnUtils nextPutAll: representation on: stream