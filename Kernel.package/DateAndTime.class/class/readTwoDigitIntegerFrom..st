input
readTwoDigitIntegerFrom: stream
	"Parse and return a decimal number of 2 digits from stream.
	Fail if that is not possible"

	| integer |
	integer := 0.
	2 timesRepeat: [ | char |
		char := stream next.
		('0123456789' includes: char) ifFalse: [ self error: 'Decimal digit expected' ].
		integer := (integer * 10) + char digitValue ].
	^ integer