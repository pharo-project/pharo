parsing-public
nextInteger
	"Main method for reading an Integer.
	This won't try to read a Float nor a ScaledDecimal."
	
	base := 10.
	neg := self peekSignIsMinus.
	integerPart := self nextUnsignedIntegerOrNilBase: base.
	integerPart ifNil: [self fail].
	(sourceStream peekFor: $r)
		ifTrue: ["<base>r<integer>"
			(base := integerPart) < 2
				ifTrue: ["A radix currently need to be greater than 1, ungobble the r and return the integer part"
					sourceStream skip: -1.
					^neg
						ifTrue: [base negated]
						ifFalse: [base]].
			self peekSignIsMinus ifTrue: [neg := neg not].
			integerPart := self nextUnsignedIntegerOrNilBase: base.
			integerPart ifNil: [self fail]].
	neg
		ifTrue: [integerPart := integerPart negated].
	self readExponent
		ifTrue: [^integerPart * (base raisedToInteger: exponent)].
	^ integerPart