Extension { #name : 'SmallInteger' }

{ #category : '*System-NumberPrinting' }
SmallInteger >> numberOfDigitsInBase: b [
	"Return how many digits are necessary to print this number in base b.
	Mostly same as super but an optimized version for base 10 case"

	b = 10 ifFalse: [^super numberOfDigitsInBase: b].
	self < 0 ifTrue: [^self negated numberOfDigitsInBase: b].
	^self decimalDigitLength
]

{ #category : '*System-NumberPrinting' }
SmallInteger >> printOn: aStream base: b nDigits: n [
	"Append a representation of this number in base b on aStream using nDigits.
	self must be positive."

	self printOn: aStream base: b length: n padded: true
]

{ #category : '*System-NumberPrinting' }
SmallInteger >> printStringBase: b [
	"Return a String representation of this number in base b.
	For SmallIntegers, it is more efficient to print directly in a String,
	rather than using a Stream like super."

	"(10 printStringBase: 10) >>> '10'"
	"(-10 printStringBase: 10) >>> '-10'"
	"(10 printStringBase: 2) >>> '1010'"
	"(10 printStringBase: 16) >>> 'A'"

	self < 0
		ifTrue: [^ '-'
				, (self negated printStringBase: b)].
	self < b
		ifTrue: [^ String
				with: (Character digitValue: self)].
	^ self printStringBase: b nDigits: (self numberOfDigitsInBase: b)
]

{ #category : '*System-NumberPrinting' }
SmallInteger >> printStringBase: b nDigits: n [
	"Return a string representation of this number in base b with n digits (left padded with 0).
	Should be invoked with: 0 <= self < (b raisedToInteger: n)."

	| integer next result |
	result := String new: n.
	integer := self.
	n to: 1 by: -1 do: [:i |
		next := integer // b.
		result byteAt: i put: (Character digitValue: (integer - (next * b))).
		integer := next].
	^result
]
