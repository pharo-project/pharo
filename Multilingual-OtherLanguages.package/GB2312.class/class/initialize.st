class methods
initialize
	"
	GB2312 initialize
"
	self initializeUcsTable.
	EncodedCharSet declareEncodedCharSet: self atIndex: 2+1.
	EncodedCharSet declareEncodedCharSet: self atIndex: 8+1.
	compoundTextSequence := String streamContents: 
		[ :stream | 
		stream nextPut: Character escape.
		stream nextPut: $$.
		stream nextPut: $(.
		stream nextPut: $A ]