parsing
readDateFrom: aStream
	"Parse a date from the given stream and answer nil if the date can't be parsed. The date may be in any of the following forms:
		<day> <monthName> <year>		(5 April 1982; 5-APR-82)
		<monthName> <day> <year>		(April 5, 1982)
		<monthNumber> <day> <year>		(4/5/82)
	In addition, the date may be preceded by the day of the week and an optional comma, such as:
		Tue, November 14, 1989"

	| day month year |
	self skipWeekdayName: aStream.
	aStream peek isDigit ifTrue: [day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue:		"month name or weekday name"
			[month := (String new: 10) writeStream.
			 [aStream peek isLetter] whileTrue: [month nextPut: aStream next].
			 month := month contents.
			 day ifNil:		"name/number..."
				[[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
				 (aStream peek isDigit) ifFalse: [^nil].
				 day := Integer readFrom: aStream]]
		ifFalse:		"number/number..."
			[month := Date nameOfMonth: day.
			 day := Integer readFrom: aStream].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	(aStream peek isDigit) ifFalse: [^nil].
	year := Integer readFrom: aStream.
	^Date year: year month: month day: day