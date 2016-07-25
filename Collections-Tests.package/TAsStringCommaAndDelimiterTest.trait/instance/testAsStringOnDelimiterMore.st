tests - as string comma delimiter sequenceable
testAsStringOnDelimiterMore

	| delim multiItemStream result allElementsAsString tmp |
	
	
	delim := ', '.
	result:=''.
	tmp:= self nonEmpty collect:[:each | each asString].
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty  asStringOn: multiItemStream delimiter: delim.
	result := multiItemStream contents.
	allElementsAsString := (result findBetweenSubstrings: delim ).
	tmp do:
		[:each |
		self assert: (tmp occurrencesOf: each)=(allElementsAsString occurrencesOf: each).
		].