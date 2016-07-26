tests - comma and delimiter
testAsStringOnDelimiterMore

	| delim multiItemStream result index |
	"delim := ', '.
	multiItemStream := '' readWrite.
	self oneTwoThreeItemCol asStringOn: multiItemStream delimiter: ', '.
	self assert: multiItemStream contents = '1, 2, 3'."
	
	delim := ', '.
	result:=''.
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty  asStringOn: multiItemStream delimiter: delim.
	result := multiItemStream contents.
	index:=1.
	(result findBetweenSubstrings: delim )do:
		[:each |
		self assert: each= ((self nonEmpty at:index)asString).
		index:=index+1
		].