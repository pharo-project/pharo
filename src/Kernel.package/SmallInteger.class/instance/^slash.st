arithmetic
/ aNumber 
	"Primitive. This primitive (for /) divides the receiver by the argument
	and returns the result if the division is exact. Fail if the result is not a
	whole integer. Fail if the argument is 0 or is not a SmallInteger. Optional.
	No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 10>
	aNumber isZero ifTrue: [^(ZeroDivide dividend: self) signal].
	^(aNumber isMemberOf: SmallInteger)
		ifTrue: [(Fraction numerator: self denominator: aNumber) reduced]
		ifFalse: [super / aNumber]