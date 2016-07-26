tests - Integer
testIntegerReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| rs |
	self assert: (NumberParser parse: '2r1e26') = (2 raisedTo: 26).
	rs := '2r1e26eee' readStream.
	self assert: (NumberParser parse: rs) = 67108864.
	self assert: rs upToEnd = 'eee'
