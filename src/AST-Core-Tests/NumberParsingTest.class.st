"
Tests to verify parsing of numbers from streams and strings.

Note: ScaledDecimalTest contains related tests for parsing ScaledDecimal.
"
Class {
	#name : #NumberParsingTest,
	#superclass : #TestCase,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #'tests - Float' }
NumberParsingTest >> testFloatFromStreamAsNumber [
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '10r-12.3456' readStream.
	aFloat := Number readFrom: rs.
	self assert: -12.3456 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2e2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs upToEnd equals: 'e2'.

	rs := '10r-12.3456d2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '-12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456q2' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456z2' readStream.
	aFloat := Number readFrom: rs.
	self assert: 12.3456 equals: aFloat.
	self assert: rs upToEnd equals: 'z2'
]

{ #category : #'tests - Float' }
NumberParsingTest >> testFloatFromStreamWithExponent [
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '1.0e-14' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs atEnd.

	rs := '1.0e-14 1' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: ' 1'.

	rs := '1.0e-14eee' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: 'eee'.

	rs := '1.0e14e10' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0e14 equals: aFloat.
	self assert: rs upToEnd equals: 'e10'.

	rs := '1.0e+14e' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e+14e'.

	rs := '1.0e' readStream.
	aFloat := Number readFrom: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e'
]

{ #category : #'tests - Float' }
NumberParsingTest >> testFloatFromStringAsNumber [
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '10r-12.3456' asNumber.
	self assert: -12.3456 equals: aFloat.
	aFloat := '10r-12.3456e2' asNumber.
	self assert: -1234.56 equals: aFloat.
	aFloat := '10r-12.3456d2' asNumber.
	self assert: -1234.56 equals: aFloat.
	aFloat := '10r-12.3456q2' asNumber.
	self assert: -1234.56 equals: aFloat.
	aFloat := '-12.3456q2' asNumber.
	self assert: -1234.56 equals: aFloat.
	aFloat := '12.3456q2' asNumber.
	self assert: 1234.56 equals: aFloat
]

{ #category : #'tests - Float' }
NumberParsingTest >> testFloatFromStringWithExponent [
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '1.0e-14' asNumber.
	self assert: 1.0e-14 equals: aFloat.
	aFloat := '1.0e-14 1' asNumber.
	self assert: 1.0e-14 equals: aFloat.
	aFloat := '1.0e-14e' asNumber.
	self assert: 1.0e-14 equals: aFloat.
	aFloat := '1.0e14e' asNumber.
	self assert: 1.0e14 equals: aFloat
]

{ #category : #'tests - Float' }
NumberParsingTest >> testFloatReadWithRadix [
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"
	| aNumber rs |
	aNumber := '2r1.0101e9' asNumber.
	self assert: 672.0 equals: aNumber.
	self assert: (Number readFrom: '2r1.0101e9') equals: 1.3125 * (2 raisedTo: 9).
	rs := '2r1.0101e9e9' readStream.
	self assert: (Number readFrom: rs) equals: 672.0.
	self assert: rs upToEnd equals: 'e9'
]

{ #category : #'tests - Integer' }
NumberParsingTest >> testIntegerFromString [
	"This covers parsing in Number>>readFrom:
	Trailing decimal points should be ignored."

	self assert: '123' asNumber equals: 123.
	self assert: '-123' asNumber equals: -123.
	self assert: '123.' asNumber equals: 123.
	self assert: '-123.' asNumber equals: -123.
	self assert: '123This is not to be read' asNumber equals: 123.
	self assert: '123s could be confused with a ScaledDecimal' asNumber equals: 123.
	self assert: '123e could be confused with a Float' asNumber equals: 123
]

{ #category : #'tests - Integer' }
NumberParsingTest >> testIntegerReadFrom [
	"Ensure remaining characters in a stream are not lost when parsing an integer."
	| rs i s |
	rs := '123s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: 's could be confused with a ScaledDecimal' equals: s.
	rs := '123.s could be confused with a ScaledDecimal' readStream.
	i := Integer readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' equals: s.
	rs := '123sA has unary message sA' readStream.
	i := Number readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: 'sA has unary message sA' equals: s.
	rs := '123sB has unary message sB' readStream.
	i := Number readFrom: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: 'sB has unary message sB' equals: s
]

{ #category : #'tests - Integer' }
NumberParsingTest >> testIntegerReadWithRadix [
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| aNumber rs |
	aNumber := '2r1e26' asNumber.
	self assert: 67108864 equals: aNumber.
	self assert: (Number readFrom: '2r1e26') equals: (2 raisedTo: 26).
	rs := '2r1e26eee' readStream.
	self assert: (Number readFrom: rs) equals: 67108864.
	self assert: rs upToEnd equals: 'eee'
]

{ #category : #'tests - Integer' }
NumberParsingTest >> testNegativeZero [
      "This test ensure that -0.0 will produce a negativeZero"

      | negativeZero |
      negativeZero := Number readFrom: '-0.0' readStream.

      "If it is a negative zero, it must behave like a negative zero... IEEE 754 tells how it should behave"
      self deny: (negativeZero at: 1) = 0 description: 'In IEEE 754, a negative zero has its sign bit set to 1'.
      self assert: negativeZero = 0 description: 'In IEEE 754, a negative zero cannot be distinguished from zero'
]

{ #category : #'tests - Float' }
NumberParsingTest >> testNumberReadExactlyError [
	"This covers parsing in Number>>readExactlyFrom:"

	| rs |

	rs := '' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := 'foo' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := '.e0' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := '-.e0' readStream.
	self should: [Number readFrom: rs] raise: Error.

	rs := '--1' readStream.
	self should: [Number readFrom: rs] raise: Error
]

{ #category : #'tests - Float' }
NumberParsingTest >> testNumberReadOnlyDigit [
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := Number readFrom: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: 'e'.

	rs := '1s' readStream.
	num := Number readFrom: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: ''.

	rs := '1.' readStream.
	num := Number readFrom: rs.
	self assert: 1 equals: num.
	self assert: num isInteger.
	self assert: rs upToEnd equals: '.'
]

{ #category : #'tests - ScaledDecimal' }
NumberParsingTest >> testScaledDecimalWithTrailingZeroes [
	"This is a non regression tests for http://bugs.squeak.org/view.php?id=7169"

	self assert: (Number readFrom: '0.50s2') equals: 1/2.
	self assert: (Number readFrom: '0.500s3') equals: 1/2.
	self assert: (Number readFrom: '0.050s3') equals: 1/20
]

{ #category : #'tests - ScaledDecimal' }
NumberParsingTest >> testScaledDecimalWithoutScaleSpecification [
	self assert: (Number readFrom: '0.050s') equals: 1/20.
	self assert: (Number readFrom: '0.050s') scale equals: 3.

	self assert: (ScaledDecimal readFrom: '0.050') equals: 1/20.
	self assert: (ScaledDecimal readFrom: '0.050') scale equals: 3
]
