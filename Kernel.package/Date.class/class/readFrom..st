importing
readFrom: aStream 
	"Read a Date from the stream in any of the forms:  
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>			(1982-04-15) (ISO8601)"
	| day month year parsedNumber prefix |
	aStream peek = $-
		ifTrue: [prefix := -1]
		ifFalse: [prefix := 1].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	aStream peek isDigit
		ifTrue: [
			parsedNumber := (Integer readFrom: aStream) * prefix.
			(parsedNumber < 0 or: [parsedNumber > 31])
				ifTrue: [year := parsedNumber]].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			month := (String new: 10) writeStream.
			[aStream peek isLetter]
				whileTrue: [month nextPut: aStream next].
			month := month contents.
			[aStream peek isAlphaNumeric]
				whileFalse: [aStream skip: 1].
			parsedNumber 
				ifNil: ["MM DD YY"
					day := Integer readFrom: aStream]
				ifNotNil: [
					year 
						ifNil: ["DD MM YY"
							day := parsedNumber]]]
		ifFalse: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			year 
				ifNil: ["MM-DD-YY or DD-MM-YY"
					parsedNumber > 12
						ifTrue: ["DD-MM-YY"
							Error signal: 'Month out of bounds: ', parsedNumber asString, '.'.
							day := parsedNumber.
							month := Month nameOfMonth: (Integer readFrom: aStream) ]
						ifFalse: ["MM-DD-YY"
							month := Month nameOfMonth: parsedNumber.
							day := Integer readFrom: aStream]]
				ifNotNil: ["YY-MM-DD"
					month := Month nameOfMonth: (Integer readFrom: aStream)]].
	[aStream peek isAlphaNumeric]
		whileFalse: [aStream skip: 1].
	year 
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0]) 
		ifTrue: [
			year < 69 
				ifTrue: [year := 2000 + year]
				ifFalse: [year := 1900 + year]].

	^ self
		year: year
		month: month
		day: day
