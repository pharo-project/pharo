tests - Float
testFloatReadWithRadix
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"
	| aNumber rs |
	aNumber := '2r1.0101e9' asNumber.
	self assert: 672.0 = aNumber.
	self assert: (Number readFrom: '2r1.0101e9') = (1.3125 * (2 raisedTo: 9)).
	rs := '2r1.0101e9e9' readStream.
	self assert: (Number readFrom: rs) = 672.0.
	self assert: rs upToEnd = 'e9'