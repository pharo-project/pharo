class methods
initialize
	"KSX1001 initialize"
	self initializeUcsTable.
	EncodedCharSet declareEncodedCharSet: self atIndex: 3+1.
	EncodedCharSet declareEncodedCharSet: self atIndex: 12+1.
	compoundTextSequence := String streamContents: 
		[ :stream | 
		stream nextPut: Character escape.
		stream nextPut: $$.
		stream nextPut: $(.
		stream nextPut: $C ]