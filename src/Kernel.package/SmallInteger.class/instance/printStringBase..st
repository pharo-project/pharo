printing
printStringBase: b 
	"Return a String representation of this number in base b.
	For SmallIntegers, it is more efficient to print directly in a String,
	rather than using a Stream like super."

	self < 0
		ifTrue: [^ '-'
				, (self negated printStringBase: b)].
	self < b
		ifTrue: [^ String
				with: (Character digitValue: self)].
	^ self printStringBase: b nDigits: (self numberOfDigitsInBase: b)