tests
testUnknown
	"Only Array for lists and Dictionary for maps are allowed for JSON encoding,
	any other Smalltalk object cannot be encoded (following the specs).
	Even considering all collections to be lists won't work because the type 
	is then lost when decoding"
	
	self should: [ STONJSON toString: 1@2 ] raise: STONWriterError.
	self should: [ STONJSON toString: #(1 2 3) asOrderedCollection ] raise: STONWriterError.