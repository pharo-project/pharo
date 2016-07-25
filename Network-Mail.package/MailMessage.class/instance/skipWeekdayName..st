parsing
skipWeekdayName: aStream
	"If the given stream starts with a weekday name or its abbreviation, advance the stream to the first alphaNumeric character following the weekday name."

	| position name abbrev |
	aStream skipSeparators.
	aStream peek isDigit
		ifTrue: [ ^ self ].
	aStream peek isLetter
		ifFalse: [ ^ self ].
	position := aStream position.
	name := (String new: 10) writeStream.
	[ aStream peek isLetter ] whileTrue: [ name nextPut: aStream next ].
	abbrev := name contents copyFrom: 1 to: (3 min: name position).
	abbrev := abbrev asLowercase.
	(#('sun' 'mon' 'tue' 'wed' 'thu' 'fri' 'sat') includes: abbrev asLowercase)
		ifTrue: [ 
			"found a weekday; skip to the next alphanumeric character"
			[ aStream peek isAlphaNumeric ] whileFalse: [ aStream skip: 1 ] ]
		ifFalse: [ 
			"didn't find a weekday so restore stream position"
			aStream position: position ]