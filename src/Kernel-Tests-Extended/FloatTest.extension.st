Extension { #name : #FloatTest }

{ #category : #'*Kernel-Tests-Extended' }
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
	  do: [:originalWord | self assert: (Float fromIEEE32Bit: originalWord) asIEEE32BitWord equals: originalWord ]
]

{ #category : #'*Kernel-Tests-Extended' }
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
	self assert: expected equals: conv.
	
	"case 2: smallest number"
	conv := 2r0.00000000000000000000001e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected equals: conv.
	
	"case 3: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.000000000000000000000011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected equals: conv.
	
	"case 4: round to nearest even also in underflow cases... here round to lower"
	conv := 2r0.000000000000000000000101e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected equals: conv.
	
	"case 5: round to nearest even also in underflow cases... here round to upper"
	conv := 2r0.0000000000000000000001011e-126 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r11.
	self assert: expected equals: conv
	
]

{ #category : #'*Kernel-Tests-Extended' }
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
	self assert: expected equals: conv.
	
	"case 2: exactly in the mid point of two 32 bit float: round toward nearest even (to upper)"
	conv := 2r1.0000000000000000000000110e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r10.
	self assert: expected equals: conv.
	
	"case 3: exactly in the mid point of two 32 bit float: round toward nearest even (to lower)"
	conv := 2r1.0000000000000000000000010e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r0.
	self assert: expected equals: conv.
	
	"case 4: obviously round to upper"
	conv := 2r1.0000000000000000000000011e25 asIEEE32BitWord.
	expected := exponentPart bitOr: 2r1.
	self assert: expected equals: conv

]

{ #category : #'*Kernel-Tests-Extended' }
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
	
	self assert: (0 arcTan: 0) equals: 0.
	self assert: (Float negativeZero arcTan: 0) equals: 0.
	self assert: ((0 arcTan: Float negativeZero) closeTo: Float pi).
	self assert: ((Float negativeZero arcTan: Float negativeZero) closeTo: Float pi negated)
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testBinaryLiteralString [

	self assert: 0.0 binaryLiteralString equals: '0.0'.
	self assert: 0.0 negated binaryLiteralString equals: '-0.0'.
	self assert: Float infinity binaryLiteralString equals: 'Float infinity'.
	self assert: Float infinity negated binaryLiteralString equals: 'Float infinity negated'.
	self assert: Float nan binaryLiteralString equals: 'Float nan'.

	self assert: Float fminDenormalized binaryLiteralString equals: '2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: Float fminNormalized binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000000e-1022'.
	self assert: (Float fminNormalized - Float fminDenormalized) binaryLiteralString equals: '2r0.1111111111111111111111111111111111111111111111111111e-1022'.
	self assert: Float epsilon binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000000e-52'.
	self assert: Float fmax binaryLiteralString equals: '2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: Float fminDenormalized negated binaryLiteralString equals: '-2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: Float fminNormalized negated binaryLiteralString equals: '-2r1.0000000000000000000000000000000000000000000000000000e-1022'.

	self assert: 1.0 binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000000e0'.
	self assert: (1.0 + Float epsilon) binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000001e0'.
	self assert: (1.0 - (Float epsilon / 2)) binaryLiteralString equals: '2r1.1111111111111111111111111111111111111111111111111111e-1'.
	self assert: 2.0 binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000000e1'.
	
	self assert: 0.1 binaryLiteralString equals: '2r1.1001100110011001100110011001100110011001100110011010e-4'.
	self assert: 0.2 binaryLiteralString equals: '2r1.1001100110011001100110011001100110011001100110011010e-3'.
	self assert: 0.3 binaryLiteralString equals: '2r1.0011001100110011001100110011001100110011001100110011e-2'.
	self assert: (0.1 + 0.2) binaryLiteralString equals: '2r1.0011001100110011001100110011001100110011001100110100e-2'.
	self assert: 0.5 binaryLiteralString equals: '2r1.0000000000000000000000000000000000000000000000000000e-1'.
	
	self assert: 2r0.0000000000000000000000000000000000000000000000000001e-1022 binaryLiteralString equals: '2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: 2r0.0101010101010101010101010101010101010101010101010101e-1022 binaryLiteralString equals: '2r0.0101010101010101010101010101010101010101010101010101e-1022'.
	self assert: 2r0.1010101010101010101010101010101010101010101010101010e-1022 binaryLiteralString equals: '2r0.1010101010101010101010101010101010101010101010101010e-1022'.
	self assert: 2r0.1111111111111111111111111111111111111111111111111111e-1022 binaryLiteralString equals: '2r0.1111111111111111111111111111111111111111111111111111e-1022'.
	self assert: 2r1.0101010101010101010101010101010101010101010101010101e-3 binaryLiteralString equals: '2r1.0101010101010101010101010101010101010101010101010101e-3'.
	self assert: 2r1.1010101010101010101010101010101010101010101010101010e3 binaryLiteralString equals: '2r1.1010101010101010101010101010101010101010101010101010e3'.
	self assert: 2r1.1111111111111111111111111111111111111111111111111111e1023 binaryLiteralString equals: '2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: -2r0.0000000000000000000000000000000000000000000000000001e-1022 binaryLiteralString equals: '-2r0.0000000000000000000000000000000000000000000000000001e-1022'.
	self assert: -2r1.0000000000000000000000000000000000000000000000000000e-1022 binaryLiteralString equals: '-2r1.0000000000000000000000000000000000000000000000000000e-1022'.
	self assert: -2r1.1111111111111111111111111111111111111111111111111111e1023 binaryLiteralString equals: '-2r1.1111111111111111111111111111111111111111111111111111e1023'.

	self assert: (Float fromIEEE64Bit: 2r0111111111110000000000000000000000000000000000000000000000000001) binaryLiteralString equals: 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r0111111111111111111111111111111111111111111111111111111111111111) binaryLiteralString equals: 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r1111111111110000000000000000000000000000000000000000000000000001) binaryLiteralString equals: 'Float nan'.
	self assert: (Float fromIEEE64Bit: 2r1111111111111111111111111111111111111111111111111111111111111111) binaryLiteralString equals: 'Float nan'

]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testCharacterization [

	"Test the largest finite representable floating point value"
	self assert: Float fmax successor equals: Float infinity.
	self assert: Float infinity predecessor equals: Float fmax.
	self assert: Float fmax negated predecessor equals: Float infinity negated.
	self assert: Float infinity negated successor equals: Float fmax negated.
	
	"Test the smallest positive representable floating point value"
	self assert: Float fmin predecessor equals: 0.0.
	self assert: 0.0 successor equals: Float fmin.
	self assert: Float fmin negated successor equals: 0.0.
	self assert: 0.0 predecessor equals: Float fmin negated.
	
	"Test the relative precision"
	self assert: Float one + Float epsilon > Float one.
	self assert: Float one + Float epsilon equals: Float one successor.
	self assert: Float one + (Float epsilon / Float radix) equals: Float one.
	
	"Test maximum and minimum exponent"
	self assert: Float fmax exponent equals: Float emax.
	self assert: Float fminNormalized exponent equals: Float emin.
	Float denormalized ifTrue: [
		self assert: Float fminDenormalized exponent equals: (Float emin + 1 - Float precision)].
	
	"Alternative tests for maximum and minimum"
	self assert: (Float radix - Float epsilon) * (Float radix raisedTo: Float emax) equals: Float fmax.
	self assert: Float epsilon * (Float radix raisedTo: Float emin) equals: Float fmin.
	
	"Test sucessors and predecessors"
	self assert: Float one predecessor successor equals: Float one.
	self assert: Float one successor predecessor equals: Float one.
	self assert: Float one negated predecessor successor equals: Float one negated.
	self assert: Float one negated successor predecessor equals: Float one negated.
	self assert: Float infinity successor equals: Float infinity.
	self assert: Float infinity negated predecessor equals: Float infinity negated.
	self assert: Float nan predecessor isNaN.
	self assert: Float nan successor isNaN.
	
	"SPECIFIC FOR IEEE 754 double precision - 64 bits"
	self assert: Float fmax hex equals: '7FEFFFFFFFFFFFFF'.
	self assert: Float fminDenormalized hex equals: '0000000000000001'.
	self assert: Float fminNormalized hex equals: '0010000000000000'.
	self assert: 0.0 hex equals: '0000000000000000'.
	self assert: Float negativeZero hex equals: '8000000000000000'.
	self assert: Float one hex equals: '3FF0000000000000'.
	self assert: Float infinity hex equals: '7FF0000000000000'.
	self assert: Float infinity negated hex equals: 'FFF0000000000000'
]

{ #category : #'*Kernel-Tests-Extended' }
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
	self deny: (1000000001 closeTo: 1000000000) description: 'exact representation are considered closeTo: only if equal'
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testComparison [
	"test equality when Float conversion loose bits"
	
	| a b c |
	a := 16r1FFFFFFFFFFFFF1.
	b := 16r1FFFFFFFFFFFFF3.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"Test equality when Float conversion exact"
	self assert: 16r1FFFFFFFFFFFFF equals: 16r1FFFFFFFFFFFFF asFloat.
	self assert: 16r1FFFFFFFFFFFFF equals: 16r1FFFFFFFFFFFFF asFloat asInteger.
	
	"Test inequality when Float conversion loose bits"
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.
	self assert: (((1 bitShift: 54)+1)/(1 bitShift: 54)) > 1.0.
	
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.
	self assert: (((1 bitShift: 54)-1)/(1 bitShift: 54)) < 1.0.
	
	"Test exact vs inexact arithmetic"
	1 to: 100 do: [:i |
		i isPowerOfTwo
			ifTrue: [self assert: (1/i) equals: (1/i) asFloat]
			ifFalse: [self deny: (1/i) equals: (1/i) asFloat]].
	
	"Test overflow (compare to infinity)"
	a := (11 raisedTo: 400) / 2.
	b := (13 raisedTo: 400) / 2.
	c := a asFloat.
	self assert: ((a = c) & (b = c)) ==> (a = b).
	
	"every integer is smaller than infinity"
	self assert: a < Float infinity.
	self assert: a > Float infinity negated.
	
	"Test underflow"
	self deny: 1 / (11 raisedTo: 400) equals: 0.
	self deny: 1 / (11 raisedTo: 400) equals: 0.0.
	
	"Test hash code"
	self assert:
		((Set new: 3) add: 3; add: 3.0; size) equals:
		((Set new: 4) add: 3; add: 3.0; size)
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testDegreeCos [

	45.0 degreeCos.	"Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeCos squared - 0.5) abs <= Float epsilon.
	self assert: (60.0 degreeCos - 0.5) abs <= Float epsilon.
	self assert: (120.0 degreeCos + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeCos closeTo: i degreesToRadians cos) ].	"Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos equals: 0.
		self assert: (k * 360 - 90) degreeCos equals: 0.
		self assert: (k * 360 + 180) degreeCos + 1 equals: 0.
		self assert: (k * 360) degreeCos - 1 equals: 0 ]
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testDegreeCosForExceptionalValues [

	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float infinity negated degreeCos isNaN
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testDegreeSin [

	45.0 degreeSin.	"Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert: (45.0 degreeSin squared - 0.5) abs <= Float epsilon.
	self assert: (30.0 degreeSin - 0.5) abs <= Float epsilon.
	self assert: (-30.0 degreeSin + 0.5) abs <= Float epsilon.
	-360.0 to: 360.0 do: [ :i | self assert: (i degreeSin closeTo: i degreesToRadians sin) ].	"Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 equals: 0.
		self assert: (k * 360 - 90) degreeSin + 1 equals: 0.
		self assert: (k * 360 + 180) degreeSin equals: 0.
		self assert: (k * 360) degreeSin equals: 0 ]
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testDegreeSinForExceptionalValues [

	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float infinity negated degreeSin isNaN
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testFloatPrinting [
	"This test shows that floats are printed exactly. The idea is too make sure that users understand that "
	
	self assert: (0.1+0.2) printString equals: '0.30000000000000004'.
	self assert: (-0.1-0.2) printString equals: '-0.30000000000000004'.
	self assert: 240 degreesToRadians cos equals: -0.5000000000000004.
  	self assert: 240 degreesToRadians cos abs equals: 0.5000000000000004
]

{ #category : #'*Kernel-Tests-Extended' }
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
	self assert: x equals: y.
	self assert: x asTrueFraction equals: int.
	
	"this one should be true for any float
	in order to conform to ISO/IEC 10967-2"
	self assert: x rounded equals: x asTrueFraction rounded.
	self assert: x negated rounded equals: x negated asTrueFraction rounded.

	"a random test"
	r := Random new.
	10000 timesRepeat: [
		x := r next * 1.9999e16 + 1.0e12 .
		self assert: x rounded equals: x asTrueFraction rounded.
		self assert: x negated rounded equals: x negated asTrueFraction rounded ]
]

{ #category : #'*Kernel-Tests-Extended' }
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
		self assert: (exactPowerOfTwo * aBitLess floorLog: 2) equals: exp - 1]
]

{ #category : #'*Kernel-Tests-Extended' }
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
			self assert: err <= (1/2)]]
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testFractionAsFloat2 [
	"test rounding to nearest even"
		
	self assert: ((1<<52)+0+(1/4)) asFloat asTrueFraction equals: ((1<<52)+0).
	self assert: ((1<<52)+0+(1/2)) asFloat asTrueFraction equals: ((1<<52)+0).
	self assert: ((1<<52)+0+(3/4)) asFloat asTrueFraction equals: ((1<<52)+1).
	self assert: ((1<<52)+1+(1/4)) asFloat asTrueFraction equals: ((1<<52)+1).
	self assert: ((1<<52)+1+(1/2)) asFloat asTrueFraction equals: ((1<<52)+2).
	self assert: ((1<<52)+1+(3/4)) asFloat asTrueFraction equals: ((1<<52)+2)
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testHugeIntegerCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=7368"

	self deny: (1.0 closeTo: 200 factorial).
	self deny: (200 factorial closeTo: 1.0).
	self assert: (Float infinity closeTo: 200 factorial) equals: (200 factorial closeTo: Float infinity)
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testInfinityCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=6729"

	self deny: (Float infinity closeTo: Float infinity negated).
	self deny: (Float infinity negated closeTo: Float infinity)
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testNaN5 [

	self assert: ((Float nan asIEEE32BitWord printPaddedWith: $0 to: 32 base: 2) copyFrom: 2 to: 9) 
		  equals: '11111111'.
	self assert: (Float fromIEEE32Bit:
		(Integer readFrom: '01111111110000000000000000000000' readStream base: 2)) isNaN
]

{ #category : #'*Kernel-Tests-Extended' }
FloatTest >> testRounding [

	self assert: (10.1234 round: 2) equals: 10.12.
 	self assert: (10.1234 round: 0) equals: 10
]
