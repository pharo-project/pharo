"
Provide tests for new clas aimed at parsing numbers.

It duplicates NumberParsingTest, with few more tests.
"
Class {
	#name : #NumberParserTest,
	#superclass : #ClassTestCase,
	#category : #'AST-Core-Tests-Parser'
}

{ #category : #utilities }
NumberParserTest >> areLowercaseDigitsAllowed [
	"Answer true if lowercase letter are allowed digits."
	
	^(NumberParser parse: '16re' onError: [-1]) = 16rE
]

{ #category : #'tests - fail' }
NumberParserTest >> testFail [
	"Verify that the value of a failblock is returned."
	self assert: (NumberParser parse: 'blablabla' onError: [42]) equals: 42
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatFromStreamAsNumber [
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '10r-12.3456' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -12.3456 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456e2e2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs upToEnd equals: 'e2'.

	rs := '10r-12.3456d2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '10r-12.3456q2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '-12.3456q2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: -1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456q2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1234.56 equals: aFloat.
	self assert: rs atEnd.

	rs := '12.3456z2' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 12.3456 equals: aFloat.
	self assert: rs upToEnd equals: 'z2'.

]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatFromStreamWithExponent [
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '1.0e-14' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs atEnd.

	rs := '1.0e-14 1' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: ' 1'.

	rs := '1.0e-14eee' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e-14 equals: aFloat.
	self assert: rs upToEnd equals: 'eee'.

	rs := '1.0e14e10' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0e14 equals: aFloat.
	self assert: rs upToEnd equals: 'e10'.

	rs := '1.0e+14e' readStream. "Plus sign is not parseable"
	aFloat := NumberParser parse: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e+14e'.

	rs := '1.0e' readStream.
	aFloat := NumberParser parse: rs.
	self assert: 1.0 equals: aFloat.
	self assert: rs upToEnd equals: 'e'.
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatGradualUnderflow [
	"Gradual underflow are tricky.
	This is a non regression test for http://bugs.squeak.org/view.php?id=6976"

	| float trueFraction str |
	
	"as a preamble, use a base 16 representation to avoid round off error and check that number parsing is correct"
	trueFraction := 16r2D2593D58B4FC4 / (16 raisedTo: 256+13).
	"Parse the number in base 16 if possible - it is impossible if lowercase letter are allowed digits due to exponent letter ambiguity."
	float := self areLowercaseDigitsAllowed
		ifFalse: [NumberParser parse: '16r2.D2593D58B4FC4e-256']
		ifTrue: [trueFraction asFloat]..
	self assert: float asTrueFraction equals: trueFraction.
	self assert: float equals: trueFraction asFloat.

	"now print in base 10"
	str := (String new: 32) writeStream.
	float absPrintExactlyOn: str base: 10.
	
	"verify if SqNumberParser can read it back"
	self assert: (NumberParser parse: str contents) equals: float. 
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatMaxAndMin [
	"This covers parsing in Number>>readFrom:"

	| rs aFloat |
	rs := '2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized equals: aFloat.
	self assert: rs atEnd.
	rs := '-2r0.0000000000000000000000000000000000000000000000000001e-1022'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fminDenormalized negated equals: aFloat.
	self assert: rs atEnd.
	rs := '2r1.1111111111111111111111111111111111111111111111111111e1023'
		readStream.
	aFloat := NumberParser parse: rs.
	self assert: Float fmax equals: aFloat.
	self assert: rs atEnd
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatPrintString [
	| f r bases |
	f := Float basicNew: 2.
	r := Random new seed: 1234567.
	"printing a Float in base other than 10 is broken if lowercase digits are allowed"
	bases := self areLowercaseDigitsAllowed ifTrue: [ #(10) ] ifFalse: [ #(2 8 10 16) ].
	100
		timesRepeat: [ f basicAt: 1 put: (r nextInt: 16r100000000) - 1.
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [ :base | 
					| str |
					str := (String new: 64) writeStream.
					f negative ifTrue: [ str nextPut: $- ].
					str
						print: base;
						nextPut: $r.
					f absPrintExactlyOn: str base: base.
					self assert: (NumberParser parse: str contents) equals: f ] ].
	"test big num near infinity"
	10
		timesRepeat: [ f basicAt: 1 put: 16r7FE00000 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [ :base | 
					| str |
					str := (String new: 64) writeStream.
					f negative ifTrue: [ str nextPut: $- ].
					str
						print: base;
						nextPut: $r.
					f absPrintExactlyOn: str base: base.
					self assert: (NumberParser parse: str contents) equals: f ] ].
	"test infinitesimal (gradual underflow)"
	10
		timesRepeat: [ f basicAt: 1 put: 0 + ((r nextInt: 16r100000) - 1).
			f basicAt: 2 put: (r nextInt: 16r100000000) - 1.
			bases
				do: [ :base | 
					| str |
					str := (String new: 64) writeStream.
					f negative ifTrue: [ str nextPut: $- ].
					str
						print: base;
						nextPut: $r.
					f absPrintExactlyOn: str base: base.
					self assert: (NumberParser parse: str contents) equals: f ] ]
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatReadError [
	"This covers parsing in Number>>readFrom:"

	| rs num |
	rs := '1e' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: 'e'.
	
	rs := '1s' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: rs upToEnd equals: ''.

	rs := '1.' readStream.
	num := NumberParser parse: rs.
	self assert: 1 equals: num.
	self assert: num isInteger.
	self assert: rs upToEnd equals: '.'.
	
	rs := '' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := 'foo' readStream.
	self should: [NumberParser parse: rs] raise: Error.

	rs := 'radix' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '-.e0' readStream.
	self should: [NumberParser parse: rs] raise: Error.
	
	rs := '--1' readStream.
	self should: [NumberParser parse: rs] raise: Error.
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatReadWithRadix [
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"
	| rs |
	self assert: (NumberParser parse: '2r1.0101e9') equals: (1.3125 * (2 raisedTo: 9)).
	rs := '2r1.0101e9e9' readStream.
	self assert: (NumberParser parse: rs) equals: 672.0.
	self assert: rs upToEnd equals: 'e9'
]

{ #category : #'tests - Float' }
NumberParserTest >> testFloatmin [
	"Note that these are originally tests cases for former bugs of libc dtoa from netlib.
	ref http://www.exploringbinary.com/gays-strtod-returns-zero-for-inputs-just-above-2-1075/
	ref http://gcc.gnu.org/viewcvs/gcc/trunk/gcc/testsuite/gcc.dg/float-exact-1.c?view=markup&pathrev=205119
	They are also non regression for a bug of NumberParser related to incorrect position of last non zero digit.
	ref https://pharo.manuscript.com/f/cases/12642/"
	| halfMin moreThanHalfmin |
	halfMin := NumberParser parse: (Float fmin asTrueFraction / 2 printShowingDecimalPlaces: 1 - Float fmin exponent).
	self assert: halfMin = 0.0 description: 'nearest even of 0.5*Float fmin is zero'.
	moreThanHalfmin := NumberParser parse: (Float fmin asTrueFraction / 2 + (10 raisedTo: Float fmin exponent - 4) printShowingDecimalPlaces: 4 - Float fmin exponent).
	self assert: moreThanHalfmin = Float fmin description: 'nearest Float of a Fraction > 0.5*Float fmin is Float fmin'.
]

{ #category : #'tests - Integer' }
NumberParserTest >> testIntegerReadFrom [
	"Ensure remaining characters in a stream are not lost when parsing an integer."
	| rs i s |
	rs := '123s could be confused with a ScaledDecimal' readStream.
	i := NumberParser parse: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: ' could be confused with a ScaledDecimal' equals: s.
	rs := '123.s could be confused with a ScaledDecimal' readStream.
	i := NumberParser parse: rs.
	self assert: i equals: 123.
	s := rs upToEnd.
	self assert: '.s could be confused with a ScaledDecimal' equals: s
]

{ #category : #'tests - Integer' }
NumberParserTest >> testIntegerReadWithRadix [
	"This covers parsing in Number>>readFrom:
	Note: In most Smalltalk dialects, the radix notation is not used for numbers
	with exponents. In Squeak, a string with radix and exponent can be parsed,
	and the exponent is always treated as base 10 (not the base indicated in the
	radix prefix). I am not sure if this is a feature, a bug, or both, but the
	Squeak behavior is documented in this test. -dtl"

	| rs |
	self assert: (NumberParser parse: '2r1e26') equals: (2 raisedTo: 26).
	rs := '2r1e26eee' readStream.
	self assert: (NumberParser parse: rs) equals: 67108864.
	self assert: rs upToEnd equals: 'eee'

]

{ #category : #'tests - Float' }
NumberParserTest >> testIntegerWithNegExponentIsAFloat [
	"Make sure a float literal like 1e(some possible neg exponent)
	isn't evaluated to the non-literal Fraction"

	| rs aFloat |
	rs := '1e-14' readStream.
	aFloat := NumberParser parse: rs.
	self assert: aFloat isFloat.

	rs := '1e-14' readStream.
	aFloat := (NumberParser on: rs ) nextNumberBase: 10.
	self assert: aFloat isFloat.


]

{ #category : #'tests - Float' }
NumberParserTest >> testIsNumber [

	self assert: (NumberParser isNumber: '-1.2') equals: true.
	self assert: (NumberParser isNumber: '2e-2') equals: true.
	
	self assert: (NumberParser isNumber: '') equals: false.
	self assert: (NumberParser isNumber: '2a') equals: false.
	self assert: (NumberParser isNumber: '--1') equals: false.
	self assert: (NumberParser isNumber: '1-') equals: false.
	self assert: (NumberParser isNumber: '1..2') equals: false.
]

{ #category : #'tests - ScaledDecimal' }
NumberParserTest >> testScaledDecimalWithTrailingZeroes [
	"This is a non regression tests for http://bugs.squeak.org/view.php?id=7169"
	
	self assert: (NumberParser parse: '0.50s2') equals: 1/2.
	self assert: (NumberParser parse: '0.500s3') equals: 1/2.
	self assert: (NumberParser parse: '0.050s3') equals: 1/20.
]

{ #category : #'tests - ScaledDecimal' }
NumberParserTest >> testScaledDecimalWithoutScaleSpecification [
	self assert: (NumberParser parse: '0.050s') equals: 1/20.
	self assert: (NumberParser parse: '0.050s') scale equals: 3.
]

{ #category : #'tests - squeezing' }
NumberParserTest >> testSqueezingOutNumbers [
	"test that SqNumberParser squeezeNumberOutOfString finds numbers."
	
	self assert: '123blabla' squeezeOutNumber equals: 123.
	self assert: 'blabla123' squeezeOutNumber equals: 123.
	self assert: 'blabla12blabla' squeezeOutNumber equals: 12.
	self assert: ('12.3bla' squeezeOutNumber -12.3 ) abs < 0.0001.
	self assert: '.1' squeezeOutNumber > 0.
	
	self assert: 'blabla1230' squeezeOutNumber equals: 1230.
]

{ #category : #'tests - Integer' }
NumberParserTest >> testcheckForCoverage [	
	"Tests for old semantics of Number>>readFrom:"
	
	self should: [(NumberParser parse: '.') = 0 ] raise: Error.
	self should: [(NumberParser parse: '.1') asNumber ] raise: Error.
	self assert: (NumberParser parse: '0.0') asNumber equals: 0.
	self assert: (NumberParser parse: '0.1') asNumber equals: 0.1.
	self assert: (NumberParser parse: '1.1') asNumber equals: 1.1.
	self assert: (NumberParser parse: '-1') asNumber equals: -1.
]
