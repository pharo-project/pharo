tests - Float
testIntegerWithNegExponentIsAFloat
	"Make sure a float literal like 1e(some possible neg exponent)
	isn't evaluated to the non-literal Fraction"

	| rs aFloat |
	rs := '1e-14' readStream.
	aFloat := NumberParser parse: rs.
	self assert: aFloat isFloat.

	rs := '1e-14' readStream.
	aFloat := (NumberParser on: rs ) nextNumberBase: 10.
	self assert: aFloat isFloat.

