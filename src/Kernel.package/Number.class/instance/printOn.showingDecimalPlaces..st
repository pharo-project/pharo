printing
printOn: aStream showingDecimalPlaces: placesDesired
	"Print a representation of the receiver on aStream in decimal notation with prescribed number of places after decimal separator."

	| rounder rounded roundedFractionPart |
	placesDesired <= 0 ifTrue: [^ self rounded printOn: aStream].
	rounder := 10 raisedToInteger: placesDesired.
	rounded := self roundTo: rounder reciprocal.
	rounded negative ifTrue: [aStream nextPut: $-].
	rounded := rounded abs.
	rounded integerPart truncated printOn: aStream.
	aStream nextPut: $..
	roundedFractionPart := (rounded fractionPart * rounder) truncated.
	roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true