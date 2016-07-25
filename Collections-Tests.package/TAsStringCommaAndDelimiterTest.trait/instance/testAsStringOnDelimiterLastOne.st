tests - as string comma delimiter sequenceable
testAsStringOnDelimiterLastOne

	| delim oneItemStream result |
	
	delim := ', '.
	result:=''.
	oneItemStream := ReadWriteStream on: result.
	self nonEmpty1Element  asStringOn: oneItemStream delimiter: delim last: 'and'.
	oneItemStream  do:
		[:each1 |
		self nonEmpty1Element do: [:each2 |self assert: each1 = (each2 asString) ]
		 ].
	

