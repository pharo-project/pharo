Extension { #name : 'Number' }

{ #category : '*System-NumberPrinting' }
Number >> printOn: aStream base: base [
	"This method should print a representation of the number for the given base,
	excluding the base prefix (and the letter r for radix)"

	self subclassResponsibility
]

{ #category : '*System-NumberPrinting' }
Number >> printOn: aStream showingDecimalPlaces: placesDesired [
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
]

{ #category : '*System-NumberPrinting' }
Number >> printShowingDecimalPlaces: placesDesired [
	"Print the receiver showing precisely the given number of places desired.  If placesDesired is positive, a decimal point and that many digits after the decimal point will always be shown.  If placesDesired is zero, a whole number will be shown, without a decimal point. Here are some examples:"

	"(23 printShowingDecimalPlaces: 2) >>> '23.00'"
	"(23.5698 printShowingDecimalPlaces: 2) >>> '23.57'"
	"(-234.567 printShowingDecimalPlaces: 5) >>> '-234.56700'"
	"(23.4567 printShowingDecimalPlaces: 0) >>> '23'"
	"(-23.4567 printShowingDecimalPlaces: 0) >>> '-23'"
	"(100000000 printShowingDecimalPlaces: 1) >>> '100000000.0'"
	"(0.98 printShowingDecimalPlaces: 5) >>> '0.98000'"
	"(-0.98 printShowingDecimalPlaces: 2) >>> '-0.98'"
	"(2.567 printShowingDecimalPlaces: 2) >>> '2.57'"
	"(-2.567 printShowingDecimalPlaces: 2) >>> '-2.57'"
	"(0 printShowingDecimalPlaces: 2) >>> '0.00'"

	^String new: placesDesired + 10 streamContents: [:aStream |
		self printOn: aStream showingDecimalPlaces: placesDesired]
]

{ #category : '*System-NumberPrinting' }
Number >> storeOn: aStream [
	self printOn: aStream
]

{ #category : '*System-NumberPrinting' }
Number >> storeOn: aStream base: base [
	"This method should print a representation of the number for the given base,
	including the base prefix (with letter r for radix)"

	^self subclassResponsibility
]

{ #category : '*System-NumberPrinting' }
Number >> storeStringBase: base [
	"Returns a string representation of the receiver in base 16 in a way that executing it will return the receiver."
	"(10 storeStringBase: 16) >>> '16rA'"
	"(10 storeStringBase: 10) >>> '10'"

	^ String streamContents: [:strm | self storeOn: strm base: base]
]
