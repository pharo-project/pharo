deprecated
readExponent: baseValue base: base from: aStream
	"Complete creation of a number, reading exponent from aStream. Answer the
	number, or nil if parsing fails.
	<number>(e|d|q)<exponent>>"

	| sign exp value |
	('edq' includes: aStream next) ifFalse: [^ nil].
	sign := ((aStream peek) == $-)
		ifTrue: [aStream next. -1]
		ifFalse: [1].
	(aStream atEnd or: [(aStream peek digitValue between: 0 and: 9) not])
		ifTrue: [^ nil]. "Avoid throwing an error"
	exp := (Integer readFrom: aStream base: 10) * sign.
	value := baseValue * (base raisedTo: exp).
	^ value
