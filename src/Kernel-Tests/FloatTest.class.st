"
I provide a test suite for Float values. Examine my tests to see how Floats should behave, and see how to use them.
"
Class {
	#name : #FloatTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #'tests - IEEE 754' }
FloatTest >> test32bitConversion [
	"Except for NaN, we can convert a 32bits float to a 64bits float exactly.
	Thus we can convert the 64bits float to the original 32bits float pattern."
	
	#(16r0 "zero"
	 16r80000000 "negative zero"
	 16r1 "min unormalized"
	 16r12345 "a unnormalized"
	 16r801FEDCB "a negative unnormalized"
	 16r7FFFFF "largest unnormalized"
	 16r800000 "smallest normalized"
	 16r468ACDEF "a normalized float"
	 16rCABD1234 "a negative normalized float"
	 16r7F7FFFFF "largest finite float"
	 16r7F800000 "positive infinity"
	 16rFF800000 "negative infinity"
	)
	  do: [:originalWord | self assert: (Float fromIEEE32Bit: originalWord) asIEEE32BitWord = originalWord]
]

{ #category : #'tests - IEEE 754' }
FloatTest >> test32bitGradualUnderflow [
	"method asIEEE32BitWord did not respect IEEE gradual underflow"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-126) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"case 1: This example is the first gradual underflow case"
	conv := 2r0.11111111111111111111111e-126 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1 or 0)"
	exponentPart := 0.
	expected := exponentPart bitOr: 2r11111111111111111111111.
	self assert: expected = conv.
	
	"case 2: smallest number"
	conv := 2r0.00000000000000000000001e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.
	
	"case 3: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.000000000000000000000011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 4: round to nearest even also in underflow cases... here round to lower"
	conv := 2r0.000000000000000000000101e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 5: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.0000000000000000000001011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r11.
	self assert: expected = conv.
	
]

{ #category : #'tests - IEEE 754' }
FloatTest >> test32bitRoundingMode [
	"method asIEEE32BitWord did not respect IEEE default rounding mode"
	
	| conv expected exponentPart |
	
	"IEEE 32 bits Float have 1 bit sign/8 bit exponent/23 bits of mantissa after leading 1
	2r1.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2reeeeeeee-127) * sign
	except when 2reeeeeeee isZero, which is a gradual underflow:
	2r0.mmmmmmmmmmmmmmmmmmmmmmm * (2 raisedTo: 2r00000000-127) * sign
	and when 2reeeeeeee = 255, which is infinity if mantissa all zero or nan otherwise"
	
	"This example has two extra bits in mantissa for testing rounding mode
	case 1: should obviously round to upper"
	conv := 2r1.0000000000000000000000111e25 asIEEE32BitWord.
	
	"expected float encoded as sign/exponent/mantissa (whithout leading 1)"
	exponentPart := 25+127 bitShift: 23. "127 is 2r01111111 or 16r7F"
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 2: exactly in the mid point of two 32 bit float: round toward nearest even (to upper)"
	conv := 2r1.0000000000000000000000110e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected = conv.
	
	"case 3: exactly in the mid point of two 32 bit float: round toward nearest even (to lower)"
	conv := 2r1.0000000000000000000000010e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r0.
	self assert: expected = conv.
	
	"case 4: obviously round to upper"
	conv := 2r1.0000000000000000000000011e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected = conv.

]

{ #category : #'tests - mathematical functions' }
FloatTest >> testArcTan [

	self assert: ((100 arcTan: 100) closeTo: Float pi / 4).
	self assert: ((-100 arcTan: 100) closeTo: Float pi / -4).
	self assert: ((100 arcTan: -100) closeTo: Float pi * 3 / 4).
	self assert: ((-100 arcTan: -100) closeTo: Float pi * -3 / 4).
	self assert: ((0 arcTan: 100) closeTo: 0).
	self assert: ((0 arcTan: -100) closeTo: Float pi).
	self assert: ((100 arcTan: 0) closeTo: Float pi / 2).
	self assert: ((-100 arcTan: 0) closeTo: Float pi / -2).
	
	self assert: ((Float negativeZero arcTan: 100) closeTo: 0).
	self assert: ((Float negativeZero arcTan: -100) closeTo: Float pi * -1).
	
	self assert: (0 arcTan: 0) = 0.
	self assert: (Float negativeZero arcTan: 0) = 0.
	self assert: ((0 arcTan: Float negativeZero) closeTo: Float pi).
	self assert: ((Float negativeZero arcTan: Float negativeZero) closeTo: Float pi negated). 
]

{ #category : #'tests - printing' }
FloatTest >> testBinaryLiteralString [

	self assert: 0.0 binaryLiteralString = '0.0'.
	self assert: 0.0 negated binaryLiteralString = '-0.0'.
	self assert: Float infinity binaryLiteralString = 'Float infinity'.
	self assert: Float infinity negated binaryLiteralString = 'Float infinity negated'.
	self assert: Float nan binaryLiteralString = 'Float nan'.

	self assert: Float fminDenormalized binaryLiteralString = '2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: Float fminNormalized binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000000e-1022'.
	self assert: (Float fminNormalized - Float fminDenormalized) binaryLiteralString = '2r0.1111111111111111111111111111111111111111111111111111e-1022'.
	self assert: Float epsilon binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000000e-52'.
	self assert: Float fmax binaryLiteralString = '2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: Float fminDenormalized negated binaryLiteralString = '-2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: Float fminNormalized negated binaryLiteralString = '-2r1.0000000000000000000000000000000000000000000000000000e-1022'.

	self assert: 1.0 binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000000e0'.
	self assert: (1.0 + Float epsilon) binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000001e0'.
	self assert: (1.0 - (Float epsilon / 2)) binaryLiteralString = '2r1.1111111111111111111111111111111111111111111111111111e-1'.
	self assert: 2.0 binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000000e1'.
	
	self assert: 0.1 binaryLiteralString = '2r1.1001100110011001100110011001100110011001100110011010e-4'.
	self assert: 0.2 binaryLiteralString = '2r1.1001100110011001100110011001100110011001100110011010e-3'.
	self assert: 0.3 binaryLiteralString = '2r1.0011001100110011001100110011001100110011001100110011e-2'.
	self assert: (0.1 + 0.2) binaryLiteralString = '2r1.0011001100110011001100110011001100110011001100110100e-2'.
	self assert: 0.5 binaryLiteralString = '2r1.0000000000000000000000000000000000000000000000000000e-1'.
	
	self assert: 2r0.0000000000000000000000000000000000000000000000000001e-1022 binaryLiteralString = '2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: 2r0.0101010101010101010101010101010101010101010101010101e-1022 binaryLiteralString = '2r0.0101010101010101010101010101010101010101010101010101e-1022'.
	self assert: 2r0.1010101010101010101010101010101010101010101010101010e-1022 binaryLiteralString = '2r0.1010101010101010101010101010101010101010101010101010e-1022'.
	self assert: 2r0.1111111111111111111111111111111111111111111111111111e-1022 binaryLiteralString = '2r0.1111111111111111111111111111111111111111111111111111e-1022'.
	self assert: 2r1.0101010101010101010101010101010101010101010101010101e-3 binaryLiteralString = '2r1.0101010101010101010101010101010101010101010101010101e-3'.
	self assert: 2r1.1010101010101010101010101010101010101010101010101010e3 binaryLiteralString = '2r1.1010101010101010101010101010101010101010101010101010e3'.
	self assert: 2r1.1111111111111111111111111111111111111111111111111111e1023 binaryLiteralString = '2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: -2r0.0000000000000000000000000000000000000000000000000001e-1022 binaryLiteralString = '-2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: -2r1.0000000000000000000000000000000000000000000000000000e-1022 binaryLiteralString = '-2r1.0000000000000000000000000000000000000000000000000000e-1022'.
	self assert: -2r1.1111111111111111111111111111111111111111111111111111e1023 binaryLiteralString = '-2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: (Float fromIEEE64Bit: 2r0111111111110000000000000000000000000000000000000000000000000001) binaryLiteralString = 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r0111111111111111111111111111111111111111111111111111111111111111) binaryLiteralString = 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r1111111111110000000000000000000000000000000000000000000000000001) binaryLiteralString = 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r1111111111111111111111111111111111111111111111111111111111111111) binaryLiteralString = 'Float nan'.

]

{ #category : #'tests - conversion' }
FloatTest >> testCeiling [
	self assert: 1.0 ceiling = 1.
	self assert: 1.1 ceiling = 2.
	self assert: -2.0 ceiling = -2.
	self assert: -2.1 ceiling = -2.
]

{ #category : #'tests - characterization' }
FloatTest >> testCharacterization [

	"Test the largest finite representable floating point value"
	self assert: Float fmax successor = Float infinity.
	self assert: Float infinity predecessor = Float fmax.
	self assert: Float fmax negated predecessor = Float infinity negated.
	self assert: Float infinity negated successor = Float fmax negated.
	
	"Test the smallest positive representable floating point value"
	self assert: Float fmin predecessor = 0.0.
	self assert: 0.0 successor = Float fmin.
	self assert: Float fmin negated successor = 0.0.
	self assert: 0.0 predecessor = Float fmin negated.
	
	"Test the relative precision"
	self assert: Float one + Float epsilon > Float one.
	self assert: Float one + Float epsilon = Float one successor.
	self assert: Float one + (Float epsilon / Float radix) = Float one.
	
	"Test maximum and minimum exponent"
	self assert: Float fmax exponent = Float emax.
	self assert: Float fminNormalized exponent = Float emin.
	Float denormalized ifTrue: [
		self assert: Float fminDenormalized exponent = (Float emin + 1 - Float precision)].
	
	"Alternative tests for maximum and minimum"
	self assert: (Float radix - Float epsilon) * (Float radix raisedTo: Float emax) = Float fmax.
	self assert: Float epsilon * (Float radix raisedTo: Float emin) = Float fmin.
	
	"Test sucessors and predecessors"
	self assert: Float one predecessor successor = Float one.
	self assert: Float one successor predecessor = Float one.
	self assert: Float one negated predecessor successor = Float one negated.
	self assert: Float one negated successor predecessor = Float one negated.
	self assert: Float infinity successor = Float infinity.
	self assert: Float infinity negated predecessor = Float infinity negated.
	self assert: Float nan predecessor isNaN.
	self assert: Float nan successor isNaN.
	
	"SPECIFIC FOR IEEE 754 double precision - 64 bits"
	self assert: Float fmax hex = '7FEFFFFFFFFFFFFF'.
	self assert: Float fminDenormalized hex = '0000000000000001'.
	self assert: Float fminNormalized hex = '0010000000000000'.
	self assert: 0.0 hex = '0000000000000000'.
	self assert: Float negativeZero hex = '8000000000000000'.
	self assert: Float one hex = '3FF0000000000000'.
	self assert: Float infinity hex = '7FF0000000000000'.
	self assert: Float infinity negated hex = 'FFF0000000000000'.
]

{ #category : #'tests - compare' }
FloatTest >> testCloseTo [
	self deny: (Float nan closeTo: Float nan) description: 'NaN isn''t close to anything'.
	self deny: (Float nan closeTo: 1.0) description: 'NaN isn''t close to anything'.
	self deny: (1.0 closeTo: Float nan) description: 'NaN isn''t close to anything'.
	
	self deny: (-1.0 closeTo: 1.0).
	self deny: (1.0 closeTo: Float infinity).
	self assert: (Float infinity closeTo: Float infinity) description: 'since they are =, they also are closeTo:'.
	
	self assert: (1.0/3.0 closeTo: 1/3).
	self assert: (1.0e-8 closeTo: 0).
	self assert: (0 closeTo: 1.0e-8).
	self assert: (1+1.0e-8 closeTo: 1.0).
	
	self assert: (1000000001.0 closeTo: 1000000000.0).
	self deny: (1000000001 closeTo: 1000000000) description: 'exact representation are considered closeTo: only if equal'.
]

{ #category : #'tests - compare' }
FloatTest >> testComparison [
	
	"test equality when Float conversion loose bits"
	| a b c |
	a := 16r1FFFFFFFFFFFFF1.
	b := 16r1FFFFFFFFFFFFF3.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"Test equality when Float conversion exact"
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat.
	self assert: 16r1FFFFFFFFFFFFF = 16r1FFFFFFFFFFFFF asFloat asInteger.
	
	"Test inequality when Float conversion loose bits"
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.0.
	
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.0.
	
	"Test exact vs inexact arithmetic"
	(1 to: 100) do: [:i |
		i isPowerOfTwo
			ifTrue: [self assert: (1/i) = (1/i) asFloat]
			ifFalse: [self deny: (1/i) = (1/i) asFloat]].
	
	"Test overflow (compare to infinity)"
	a := (11 raisedTo: 400) / 2.
	b := (13 raisedTo: 400) / 2.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"every integer is smaller than infinity"
	self assert: a < Float infinity.
	self assert: a > Float infinity negated.
	
	"Test underflow"
	self deny: 1 / (11 raisedTo: 400) = 0.
	self deny: 1 / (11 raisedTo: 400) = 0.0.
	
	"Test hash code"
	self assert:
		((Set new: 3) add: 3; add: 3.0; size) =
		((Set new: 4) add: 3; add: 3.0; size).
]

{ #category : #'tests - compare' }
FloatTest >> testComparisonWhenPrimitiveFails [
	"This is related to http://bugs.squeak.org/view.php?id=7361"

	self deny: 0.5 < (1/4).
	self deny: 0.5 < (1/2).
	self assert: 0.5 < (3/4).
	
	self deny: 0.5 <= (1/4).
	self assert: 0.5 <= (1/2).
	self assert: 0.5 <= (3/4).
	
	self assert: 0.5 > (1/4).
	self deny: 0.5 > (1/2).
	self deny: 0.5 > (3/4).
	
	self assert: 0.5 >= (1/4).
	self assert: 0.5 >= (1/2).
	self deny: 0.5 >= (3/4).
	
	self deny: 0.5 = (1/4).
	self assert: 0.5 = (1/2).
	self deny: 0.5 = (3/4).
	
	self assert: 0.5 ~= (1/4).
	self deny: 0.5 ~= (1/2).
	self assert: 0.5 ~= (3/4).
]

{ #category : #'tests - arithmetic' }
FloatTest >> testContinuedFractions [
	self assert: (Float pi asApproximateFractionAtOrder: 1) = (22/7).
	self assert: (Float pi asApproximateFractionAtOrder: 3) = (355/113)
]

{ #category : #tests }
FloatTest >> testCopy [
	"Elementary tests"
	self assert: 2.0 copy = 2.0.
	self assert: -0.5 copy = -0.5.
	
	"Are exceptional Floats preserved by the copy ?"
	self assert: Float nan copy isNaN.
	self assert: Float infinity copy = Float infinity.
	self assert: Float infinity negated copy = Float infinity negated.
	
	"Is the sign of zero preserved by the copy ?"
	self assert: 0.0 copy hex = 0.0 hex.
	self assert: Float negativeZero copy hex = Float negativeZero hex.
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testCopySignTo [

      "Set up"
      | negatives negz positives strictNegatives strictPositives zero |
      strictPositives := {2. 2.5. Float infinity}.
      strictNegatives := {-3. -3.25. Float infinity negated}.
      zero := 0.0.
      negz := Float negativeZero.
      positives := strictPositives copyWith: zero.
      negatives := strictNegatives copyWith: negz.

      "Test the copy sign functions"
      positives do: [:aPositiveSign |
              positives do: [:aPositive |
                      self assert: (aPositiveSign copySignTo: aPositive) = aPositive].
              negatives do: [:aNegative |
                      self assert: (aPositiveSign copySignTo: aNegative) = aNegative negated].
              (aPositiveSign copySignTo: zero) sign = 0.
              (aPositiveSign copySignTo: negz) sign = 0].

      negatives do: [:aNegativeSign |
              positives do: [:aPositive |
                      self assert: (aNegativeSign copySignTo: aPositive) = aPositive negated].
              negatives do: [:aNegative |
                      self assert: (aNegativeSign copySignTo: aNegative) = aNegative].
              (aNegativeSign copySignTo: zero) sign = -1.
              (aNegativeSign copySignTo: negz) sign = -1].
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testDegreeCos [
	45.0 degreeCos.	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeCos squared - 0.5) abs <= Float epsilon.
	self assert: (60.0 degreeCos - 0.5) abs <= Float epsilon.
	self assert: (120.0 degreeCos + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ].	"Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos = 0.
		self assert: (k * 360 - 90) degreeCos = 0.
		self assert: (k * 360 + 180) degreeCos + 1 = 0.
		self assert: (k * 360) degreeCos - 1 = 0 ]
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testDegreeCosForExceptionalValues [
	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float infinity negated degreeCos isNaN.
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testDegreeSin [
	45.0 degreeSin.	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeSin squared - 0.5) abs <= Float epsilon.
	self assert: (30.0 degreeSin - 0.5) abs <= Float epsilon.
	self assert: (-30.0 degreeSin + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ].	"Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 = 0.
		self assert: (k * 360 - 90) degreeSin + 1 = 0.
		self assert: (k * 360 + 180) degreeSin = 0.
		self assert: (k * 360) degreeSin = 0 ]
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testDegreeSinForExceptionalValues [
	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float infinity negated degreeSin isNaN.
]

{ #category : #'tests - arithmetic' }
FloatTest >> testDivide [

	self assert: 1.5 / 2.0 = 0.75.
	
	self assert: 2.0 / 1 = 2.0.
	
	self should: [ 2.0 / 0 ] raise: ZeroDivide.
	self should: [ 2.0 / 0.0 ] raise: ZeroDivide.
	self should: [ 1.2 / Float negativeZero ] raise: ZeroDivide.
	self should: [ 1.2 / (1.3 - 1.3) ] raise: ZeroDivide
	
]

{ #category : #'tests - printing' }
FloatTest >> testFloatPrintPolicy [
	"It is hard to test printing Floats reliably, but this at least covers the code path"

	| pi |
	
	pi := FloatPrintPolicy value: InexactFloatPrintPolicy new during: [ Float pi printString ].
	self assert: (pi beginsWith: '3.14159').
	
	pi := FloatPrintPolicy value: ExactFloatPrintPolicy new during: [ Float pi printString ].
	self assert: (pi beginsWith: '3.14159').
]

{ #category : #'tests - conversion' }
FloatTest >> testFloatPrinting [
	"This test shows that floats are printed exactly. The idea is too make sure that users understand that "
	
	self assert: (0.1+0.2) printString = '0.30000000000000004'.
	self assert: (-0.1-0.2) printString =  '-0.30000000000000004'.
	self assert: 240 degreesToRadians cos =  -0.5000000000000004.
  	self assert: 240 degreesToRadians cos abs = 0.5000000000000004
]

{ #category : #'tests - conversion' }
FloatTest >> testFloatRounded [
	"5000000000000001 asFloat has an exact representation (no round off error).
	It should round to nearest integer without loosing bits.
	This is a no regression test on http://bugs.squeak.org/view.php?id=7134"
	
	| x y int r |
	
	"This is a preamble asserting exactness of representation
	and quality of various conversions"
	int := 5000000000000001.
	x := int asFloat.
	y := (5 asFloat squared squared squared squared timesTwoPower: 15) + 1.
	self assert: x = y.
	self assert: x asTrueFraction = int.
	
	"this one should be true for any float
	in order to conform to ISO/IEC 10967-2"
	self assert: x rounded = x asTrueFraction rounded.
	self assert: x negated rounded = x negated asTrueFraction rounded.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x rounded = x asTrueFraction rounded.
		self assert: x negated rounded = x negated asTrueFraction rounded]
]

{ #category : #'tests - conversion' }
FloatTest >> testFloatTruncated [
	"(10 raisedTo: 16) asFloat has an exact representation (no round off error).
	It should convert back to integer without loosing bits.
	This is a no regression test on http://bugs.impara.de/view.php?id=3504"
	
	| x y int r |
	int := 10 raisedTo: 16.
	x := int asFloat.
	y := (5 raisedTo: 16) asFloat timesTwoPower: 16.
	self assert: x = y.
	
	self assert: x asInteger = int.
	
	"this one should be true for any float"
	self assert: x asInteger = x asTrueFraction asInteger.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x truncated = x asTrueFraction truncated]
]

{ #category : #'tests - conversion' }
FloatTest >> testFloor [
	self assert: 1.0 floor = 1.
	self assert: 1.1 floor = 1.
	self assert: -2.0 floor = -2.
	self assert: -2.1 floor = -3.
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testFloorLog2 [
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."
	
	| aBitLess aBitMore |
	aBitMore := 1 + Float epsilon.
	aBitLess := 1 - Float epsilon.
	Float emin + 1 to: Float emax - 1 do: [:exp |
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: (exactPowerOfTwo floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitMore floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitLess floorLog: 2) equals: exp - 1].
]

{ #category : #'tests - conversion' }
FloatTest >> testFractionAsFloat [
	"use a random test"
	
	| r m frac err collec |
	r := Random new seed: 1234567.
	m := (2 raisedTo: 54) - 1.
	200 timesRepeat: [
		frac := ((r nextInt: m) * (r nextInt: m) + 1) / ((r nextInt: m) * (r nextInt: m) + 1).
		err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
		self assert: err < (1/2)].
	
	collec := #(16r10000000000000 16r1FFFFFFFFFFFFF 1 2 16r20000000000000 16r20000000000001 16r3FFFFFFFFFFFFF 16r3FFFFFFFFFFFFE 16r3FFFFFFFFFFFFD).
	collec do: [:num |
		collec do: [:den |
			frac := Fraction numerator: num denominator: den.
			err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
			self assert: err <= (1/2)]].
]

{ #category : #'tests - conversion' }
FloatTest >> testFractionAsFloat2 [
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction = ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction = ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction = ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction = ((1<<52)+2).
]

{ #category : #tests }
FloatTest >> testFractionAsFloatWithUnderflow [
	"test rounding to nearest even"

	| underflowPower |
	underflowPower := Float emin - Float precision.
	self assert: (2 raisedTo: underflowPower) asFloat = 0.0.	
	self assert: (2 raisedTo: underflowPower) negated asFloat = 0.0.
	self assert: (2 raisedTo: underflowPower) negated asFloat signBit = 1 description: 'a negative underflow should return a negative zero'.
]

{ #category : #tests }
FloatTest >> testHash [
	self assert: 2 = 2.0 ==> (2 hash = 2.0 hash).
	self assert: 1 / 2 = 0.5 ==> ((1 / 2) hash = 0.5 hash).
	Float nan hash.
	Float infinity hash
]

{ #category : #'tests - infinity behavior' }
FloatTest >> testHugeIntegerCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=7368"
	
 	"FloatTest new testHugeIntegerCloseTo"

	self deny: (1.0 closeTo: 200 factorial).
	self deny: (200 factorial closeTo: 1.0).
	self assert: (Float infinity closeTo: 200 factorial) = (200 factorial closeTo: Float infinity).
]

{ #category : #'tests - infinity behavior' }
FloatTest >> testInfinity1 [
   "FloatTest new testInfinity1"

	| i1  i2 |

	i1 := 10000 exp.
	i2 := 1000000000 exp.
	self assert: i1 isInfinite & i2 isInfinite & (i1 = i2).
	"All infinities are equal. (This is a very substantial difference to NaN's, which are never equal."

]

{ #category : #'tests - infinity behavior' }
FloatTest >> testInfinity2 [
   "FloatTest new testInfinity2"

	| i1  i2 |
	i1 := 10000 exp.
	i2 := 1000000000 exp.
	i2 := 0 - i2. " this is entirely ok. You can compute with infinite values."

	self assert: i1 isInfinite & i2 isInfinite & i1 positive & i2 negative.
	self deny: i1 = i2.
  	"All infinities are signed. Negative infinity is not equal to Infinity"

]

{ #category : #'tests - infinity behavior' }
FloatTest >> testInfinityCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=6729:"
	
 	"FloatTest new testInfinityCloseTo"

	self deny: (Float infinity closeTo: Float infinity negated).
	self deny: (Float infinity negated closeTo: Float infinity).
]

{ #category : #'tests - conversion' }
FloatTest >> testIntegerAsFloat [
	"assert IEEE 754 round to nearest even mode is honoured"
	
	self deny: 16r1FFFFFFFFFFFF0801 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 65 bits"
	self deny: 16r1FFFFFFFFFFFF0802 asFloat = 16r1FFFFFFFFFFFF0800 asFloat. "this test is on 64 bits"
	self assert: 16r1FFFFFFFFFFF1F800 asFloat = 16r1FFFFFFFFFFF20000 asFloat. "nearest even is upper"
	self assert: 16r1FFFFFFFFFFFF0800 asFloat = 16r1FFFFFFFFFFFF0000 asFloat. "nearest even is lower"

]

{ #category : #'tests - zero behavior' }
FloatTest >> testIsZero [
	self assert: 0.0 isZero.
	self deny:  0.1 isZero.
]

{ #category : #'tests - compare' }
FloatTest >> testLiteralEqual [

	self deny: (0.0 literalEqual: 0.0 negated).
	self deny: (-0.0 literalEqual: -0.0 negated).
	self deny: (0.0 literalEqual: -0.0).

]

{ #category : #'tests - characterization' }
FloatTest >> testMaxExactInteger [
	"
	FloatTest new testMaxExactInteger
	"

	self assert: Float maxExactInteger asFloat truncated = Float maxExactInteger.
	0 to: 10000 do: [ :j |
		self assert: (Float maxExactInteger-j) asFloat truncated = (Float maxExactInteger-j) ].
	self deny: (Float maxExactInteger+1) asFloat truncated = (Float maxExactInteger+1)
	
]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaN1 [
   	"FloatTest new testNaN1"

	self assert: Float nan == Float nan.
	self deny: Float nan = Float nan.
	"a NaN is not equal to itself."

]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaN2 [
	"Two NaN values are always considered to be different.
	On an little-endian machine (32 bit Intel), Float nan is 16rFFF80000 16r00000000.
	On a big-endian machine (PowerPC), Float nan is 16r7FF80000 16r00000000. Changing
	the bit pattern of the first word of a NaN produces another value that is still
	considered equal to NaN. This test should work on both little endian and big
	endian machines. However, it is not guaranteed to work on future 64 bit versions
	of Squeak, for which Float may have different internal representations."

	"FloatTest new testNaN2"

	| nan1 nan2 |
	nan1 := Float nan copy.
	nan2 := Float nan copy.

	"test two instances of NaN with the same bit pattern"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1.

	"change the bit pattern of nan1"
	self assert: nan1 size = 2.
	self assert: (nan1 at: 2) = 0.
	nan1 at: 1 put: (nan1 at: 1) + 999.
	self assert: nan1 isNaN.
	self assert: nan2 isNaN.
	self deny: (nan1 at: 1) = (nan2 at: 1).

	"test two instances of NaN with different bit patterns"
	self deny: nan1 = nan2.
	self deny: nan1 == nan2.
	self deny: nan1 = nan1.
	self assert: nan1 == nan1

]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaN3 [
   "FloatTest new testNaN3"

   	| set item identitySet |
	set := Set new.
	set add: (item := Float nan).
	self deny: (set includes: item).
	identitySet := IdentitySet new.
	identitySet add: (item := Float nan).
	self assert: (identitySet includes: item).
	"as a NaN is not equal to itself, it can not be retrieved from a set"

]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaN4 [
   	"FloatTest new testNaN4"

	| dict |
	dict := Dictionary new.
	dict at: Float nan put: #NaN.
	self deny: (dict includes: Float nan).
	"as a NaN is not equal to itself, it can not be retrieved when it is used as a dictionary key"

]

{ #category : #'tests - IEEE 754' }
FloatTest >> testNaN5 [
	self assert: ((Float nan asIEEE32BitWord printPaddedWith: $0 to: 32 base: 2)
		copyFrom: 2 to: 9) = '11111111'.
	self assert: (Float fromIEEE32Bit:
		(Integer readFrom: '01111111110000000000000000000000' readStream base: 2)) isNaN
]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaNCompare [
	"IEEE 754 states that NaN cannot be ordered.
	As a consequence, every arithmetic comparison involving a NaN SHOULD return false.
	Except the is different test (~=).
	This test does verify this rule"
	
	| compareSelectors theNaN anotherNaN comparand brokenMethods warningMessage |
	self skip.
	compareSelectors := #(#< #<= #> #>= #=).
	theNaN := Float nan.
	anotherNaN := Float infinity - Float infinity.
	comparand := {1. 2.3. Float infinity. 2/3. 1.25s2. 2 raisedTo: 50}.
	comparand := comparand , (comparand collect: [:e | e negated]).
	comparand := comparand , {theNaN. anotherNaN}.

"do a first pass to collect all broken methods"
	brokenMethods := Set new.
	comparand do: [:comp |
		compareSelectors do: [:op |
			(theNaN perform: op with: comp) ifTrue: [brokenMethods add: (theNaN class lookupSelector: op)].
			(comp perform: op with: theNaN) ifTrue: [brokenMethods add: (comp class lookupSelector: op)]].
		(theNaN ~= comp) ifFalse: [brokenMethods add: (theNaN class lookupSelector: #~=)].
		(comp ~= theNaN) ifFalse: [brokenMethods add: (comp class lookupSelector: #~=)]].
	
"build a warning message to tell about all broken methods at once"
	warningMessage := String streamContents: [:s |
			s nextPutAll: 'According to IEEE 754 comparing with a NaN should always return false, except ~= that should return true.'; cr.
			s nextPutAll: 'All these methods failed to do so. They are either broken or call a broken one'.
			brokenMethods do: [:e | s cr; print: e methodClass; nextPutAll: '>>'; print: e selector]].
		
"Redo the tests so as to eventually open a debugger on one of the failures"
	brokenMethods := Set new.
	comparand do: [:comp2 |
		compareSelectors do: [:op2 |
			self deny: (theNaN perform: op2 with: comp2) description: warningMessage.
			self deny: (comp2 perform: op2 with: theNaN) description: warningMessage].
		self assert: (theNaN ~= comp2) description: warningMessage.
		self assert: (comp2 ~= theNaN) description: warningMessage].
]

{ #category : #'tests - NaN behavior' }
FloatTest >> testNaNisLiteral [
	self deny: Float nan isLiteral description: 'there is no literal representation of NaN'
]

{ #category : #'tests - zero behavior' }
FloatTest >> testNegativeZeroAbs [
	self assert: Float negativeZero abs sign positive description: 'the absolute value of a negative zero is zero'
]

{ #category : #'tests - zero behavior' }
FloatTest >> testNegativeZeroSign [
	self assert: Float negativeZero sign = 0. "Like any other zero, a negative zero has its sign being zero"
	self assert: Float negativeZero signBit = 1 "But it can be distinguished with its signBit"
]

{ #category : #'tests - printing' }
FloatTest >> testPrintPaddedWithTo [
	"The problem was caused by treating the format specifier as a number rather than
	as a string, such the the number may be a Float subject to floating point rounding
	errors. The solution to treat the format specifier as a string, and extract the integer
	fields before and after the decimal point in the string."

	self assert: (1.0 printPaddedWith: $0 to: 2.2) = '01.00'.
	self assert: (1.0 printPaddedWith: $X to: 2.2) = 'X1.0X'.
	self assert: (1.0 printPaddedWith: $0 to: 2) = '01.0'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2) = '12345.6789'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2.2) = '12345.6789'.
	self assert: (12.34 printPaddedWith: $0 to: 2.2) = '12.34'.
	self assert: (12345.6789 printPaddedWith: $0 to: 2.2) = '12345.6789'.
	self assert: (123.456 printPaddedWith: $X to: 4.4) = 'X123.456X'.
	self assert: (1.0 printPaddedWith: $0 to: 2.1) = '01.0'.
	self assert: (1.0 printPaddedWith: $0 to: 2.2) = '01.00'.
	self assert: (1.0 printPaddedWith: $0 to: 2.3) = '01.000'. "previously failed due to float usage"
	self assert: (1.0 printPaddedWith: $0 to: 2.4) = '01.0000'. "previously failed due to float usage"
	self assert: (1.0 printPaddedWith: $0 to: 2.5) = '01.00000'


]

{ #category : #'tests - mathematical functions' }
FloatTest >> testRaisedTo [
	"
	FloatTest new testRaisedTo
	"
	self should: [ -1.23 raisedTo: 1/4 ] raise: ArithmeticError
]

{ #category : #'tests - conversion' }
FloatTest >> testReadFromManyDigits [
	"A naive algorithm may interpret these representations as Infinity or NaN.
	This is http://bugs.squeak.org/view.php?id=6982"
	
	| s1 s2 |
	s1 := '1' , (String new: 321 withAll: $0) , '.0e-321'.
	s2 := '0.' , (String new: 320 withAll: $0) , '1e321'.
	self assert: (Number readFrom: s1) = 1.
	self assert: (Number readFrom: s2) = 1.
]

{ #category : #'tests - NaN behavior' }
FloatTest >> testReciprocal [

	self 
		assert: 1.0 reciprocal = 1.0;
		assert: 2.0 reciprocal = 0.5;
		assert: -1.0 reciprocal = -1.0;
		assert: -2.0 reciprocal = -0.5.
		
	self should: [ 0.0 reciprocal ] raise: ZeroDivide
]

{ #category : #'tests - conversion' }
FloatTest >> testRounded [
	self assert: 0.9 rounded = 1.
	self assert: 1.0 rounded = 1.
	self assert: 1.1 rounded = 1.
	self assert: -1.9 rounded = -2.
	self assert: -2.0 rounded = -2.
	self assert: -2.1 rounded = -2.
	
	"In case of tie, round to upper magnitude"
	self assert: 1.5 rounded = 2.
	self assert: -1.5 rounded = -2.
]

{ #category : #'tests - rounding' }
FloatTest >> testRounding [

	self assert: (10.1234 round: 2) = 10.12.
 	self assert: (10.1234 round: 0) = 10
]

{ #category : #'tests - compare' }
FloatTest >> testSetOfFloat [
	"Classical disagreement between hash and = did lead to a bug.
	 This is a non regression test from http://bugs.squeak.org/view.php?id=3360"
	
	| size3 size4 |
	size3 := (Set new: 3) add: 3; add: 3.0; size.
	size4 := (Set new: 4) add: 3; add: 3.0; size.
	self assert: size3 = size4 description: 'The size of a Set should not depend on its capacity.'
]

{ #category : #'tests - mathematical functions' }
FloatTest >> testSign [

      "Set up"
      | negatives negz positives strictNegatives strictPositives zero |
      strictPositives := {2. 2.5. Float infinity}.
      strictNegatives := {-3. -3.25. Float infinity negated}.
      zero := 0.0.
      negz := Float negativeZero.
      positives := strictPositives copyWith: zero.
      negatives := strictNegatives copyWith: negz.

      "The sign of non zeros"
      strictPositives do: [:aPositive | self assert: aPositive sign = 1].
      strictNegatives do: [:aNegative | self assert: aNegative sign = -1].

      "The sign of zeros"
      self assert: zero sign = 0.
      self assert: negz sign = 0.

      "Test the copy sign functions"
      positives do: [:aPositiveSign |
              positives do: [:aPositive |
                      self assert: (aPositive sign: aPositiveSign) = aPositive].
              negatives do: [:aNegative |
                      self assert: (aNegative sign: aPositiveSign) = aNegative negated].
              (zero sign: aPositiveSign) sign = 0.
              (negz sign: aPositiveSign) sign = 0.
              (zero sign: aPositiveSign) signBit = 0.
              (negz sign: aPositiveSign) signBit = 0].

      negatives do: [:aNegativeSign |
              positives do: [:aPositive |
                      self assert: (aPositive sign: aNegativeSign) = aPositive negated].
              negatives do: [:aNegative |
                      self assert: (aNegative sign: aNegativeSign) = aNegative].
              (zero sign: aNegativeSign) sign = 0.
              (negz sign: aNegativeSign) sign = 0.
              (zero sign: aNegativeSign) signBit = 1.
              (negz sign: aNegativeSign) signBit = 1].
]

{ #category : #'tests - printing' }
FloatTest >> testStoreBase16 [
	"This bug was reported in mantis http://bugs.squeak.org/view.php?id=6695"

	self
		assert: (20.0 storeStringBase: 16) = '16r14.0'
		description: 'the radix prefix should not be omitted, except in base 10'
]

{ #category : #'tests - printing' }
FloatTest >> testStoreOn [
	| float |
	float := -1.2.
	
	self assert: (String streamContents: [ :s | float storeOn: s ]) equals: ' -1.2'.
	
	float := 1.2.
	
	self assert: (String streamContents: [ :s | float storeOn: s ]) equals: '1.2'
]

{ #category : #'tests - conversion' }
FloatTest >> testStringAsNumber [
	"This covers parsing in Number>>readFrom:"

	| aFloat |
	aFloat := '10r-12.3456' asNumber.
	self assert: -12.3456 = aFloat.
	aFloat := '10r-12.3456e2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456d2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '10r-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '-12.3456q2' asNumber.
	self assert: -1234.56 = aFloat.
	aFloat := '12.3456q2' asNumber.
	self assert: 1234.56 = aFloat.

]

{ #category : #'tests - conversion' }
FloatTest >> testTruncated [
	self assert: 1.0 truncated = 1.
	self assert: 1.1 truncated = 1.
	self assert: -2.0 truncated = -2.
	self assert: -2.1 truncated = -2.
]

{ #category : #'tests - zero behavior' }
FloatTest >> testZero1 [
	"FloatTest new testZero1"

	self assert: Float negativeZero = 0 asFloat.
	self assert: (Float negativeZero at: 1) ~= (0 asFloat at: 1).

	"The negative zero has a bit representation that is different from the bit representation of the positive zero. Nevertheless, both values are defined to be equal."

]

{ #category : #'tests - arithmetic' }
FloatTest >> testZeroRaisedToNegativePower [
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0.0 raisedTo: -1] raise: ZeroDivide.
	self should: [0.0 raisedTo: -1.0] raise: ZeroDivide.
]

{ #category : #'tests - zero behavior' }
FloatTest >> testZeroSignificandAsInteger [
	"This is about http://bugs.squeak.org/view.php?id=6990"
	
	self assert: 0.0 significandAsInteger = 0
]
