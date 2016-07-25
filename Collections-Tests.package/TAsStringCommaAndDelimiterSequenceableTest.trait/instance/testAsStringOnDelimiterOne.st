tests - comma and delimiter
testAsStringOnDelimiterOne

	| delim oneItemStream result |
	"delim := ', '.
	oneItemStream := '' readWrite.
	self oneItemCol asStringOn: oneItemStream delimiter: delim.
	self assert: oneItemStream contents = '1'."
	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim.
	oneItemStream do:[:each | self assert: each = (self nonEmpty1Element first asString)].
	

	