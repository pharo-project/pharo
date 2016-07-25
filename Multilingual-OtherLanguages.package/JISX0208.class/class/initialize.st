class methods
initialize
	"self initialize"
	self initializeUcsTable.
	EncodedCharSet declareEncodedCharSet: self atIndex: 1+1.
	EncodedCharSet declareEncodedCharSet: self atIndex: 4+1.

	compoundTextSequence := String streamContents: 
		[ :s | 
		s nextPut: (Character value: 27).
		s nextPut: $$.
		s nextPut: $B ]