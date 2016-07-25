parsing stamp
parseTimestampFrom: aString default: anObject
	"Parse a date-time from a timestamp-string that is extracted from a source file. If there is no timestamp, or we cannot make sense of it, we return the default value."

	| tokens dateStartIndex unknown |
	"The following timestamp strings are supported (source: squeak sources archeological survey):
		<authorname><date><time>. 
		<authorname><date>
		<date><time>
		<date><time><authorname>
		<date><authorname>
		<historical>
	All fields can be separated by spaces or line ends but a separator between authorname and date can be missing as well"
	unknown := anObject.
	aString isEmptyOrNil
		ifTrue: [ ^ unknown ].
	dateStartIndex := (aString indexOf: $/) - 1.	"If there is no / character in the timestamp, we cannot parse a date and return the epoch"
	dateStartIndex = -1
		ifTrue: [ ^ unknown ].
	^ [ 
	"Go the start of the date string (there can be 1 or 2 digits and a space separator can be missing at the front!!)"
	(dateStartIndex >= 2 and: [ (aString at: dateStartIndex - 1) isDigit ])
		ifTrue: [ dateStartIndex := dateStartIndex - 1 ].	"Extract only those tokens that possibly represent date or time"
	tokens := (aString copyFrom: dateStartIndex to: aString size) substrings
		select: [ :token | (token occurrencesOf: $/) = 2 or: [ (token occurrencesOf: $:) = 1 ] ].	"2 tokens is a datetime"
	tokens size = 2
		ifTrue: [ (tokens joinUsing: Character space) asDateAndTime ]
		ifFalse: [ tokens first asDate asDateAndTime ] ]
		on: Exception
		do: [ :e | unknown ]