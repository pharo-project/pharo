private
readDecimalByteFrom: aStream
	"Read a positive, decimal integer from the given stream. Stop when a non-digit or end-of-stream is encountered. Return nil if stream is not positioned at a decimal digit or if the integer value read exceeds 255.
JMM - 000503 fixed didn't work correctly"

	| digitSeen value digit |
	digitSeen := false.
	value := 0.
	[aStream atEnd] whileFalse: 
		[digit := aStream next digitValue.
		(digit < 0 or: [digit > 9]) ifTrue: [
			aStream skip: -1.
			(digitSeen not or: [value > 255]) ifTrue: [^ nil].
			^ value].
		digitSeen := true.
		value := (value * 10) + digit].
	(digitSeen not or: [value > 255]) ifTrue: [^ nil].
	^ value
