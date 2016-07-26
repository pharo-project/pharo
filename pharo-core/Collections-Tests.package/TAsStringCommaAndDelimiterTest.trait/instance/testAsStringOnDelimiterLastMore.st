tests - as string comma delimiter sequenceable
testAsStringOnDelimiterLastMore

	| delim multiItemStream result last allElementsAsString tmp |
	
	delim := ', '.
	last := ' and '.
	result:=''.
	tmp := self nonEmpty collect: [:each | each asString].
	multiItemStream := ReadWriteStream on:result.
	self nonEmpty  asStringOn: multiItemStream delimiter: delim last: last.
	result := multiItemStream contents.
	allElementsAsString:=(result findBetweenSubstrings: delim ).
	tmp do:[:each |
		self assert: (tmp occurrencesOf: each) = (allElementsAsString occurrencesOf: each)].
	self assert: ((allElementsAsString at: (allElementsAsString size - 1))=('and'))