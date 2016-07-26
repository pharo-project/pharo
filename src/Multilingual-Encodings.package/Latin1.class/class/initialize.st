class methods
initialize
	"
	self initialize
"
	self initializeUcsTable.
	compoundTextSequence := String streamContents: 
		[ :s | 
		s nextPut: (Character value: 27).
		s nextPut: $(.
		s nextPut: $B ].
	rightHalfSequence := String streamContents: 
		[ :s | 
		s nextPut: (Character value: 27).
		s nextPut: $-.
		s nextPut: $A ]