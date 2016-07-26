instance creation
readFrom: strm  
	"Symbol readFromString: '#abc'"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
    	^ strm contents parseLiterals first.
 