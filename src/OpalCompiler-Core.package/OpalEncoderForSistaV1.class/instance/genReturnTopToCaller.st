bytecode generation
genReturnTopToCaller
	"94		01011110		Return Stack Top From Block [* return from enclosing block N, ExtA]"
	"If extended, the least significant bit of the extension determines if we return to the caller or not
	 and the most significant bits determine how many levels of the static chain to return from.
		ExtA = iiiiiiij
		iiiiiii=0,j=0	=>	return to caller
		iiiiiii=0,j=1	=>	illegal
		iiiiiii=1,j=0	=>	return to outerContext
		iiiiiii=1,j=1	=>	return to outerContext sender/return from outerContext
		iiiiiii=2,j=0	=>	return to outerContext outerContext
		iiiiiii=2,j=1	=>	return to outerContext outerContext sender/return from outerContext outerContext
		etc"

	stream nextPut: 94