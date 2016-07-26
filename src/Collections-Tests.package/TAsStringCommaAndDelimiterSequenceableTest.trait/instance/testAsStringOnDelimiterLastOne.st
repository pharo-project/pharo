tests - comma and delimiter
testAsStringOnDelimiterLastOne

	| delim oneItemStream result |
	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)].
	

	