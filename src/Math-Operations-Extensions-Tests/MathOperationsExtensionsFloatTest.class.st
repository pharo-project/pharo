Class {
	#name : 'MathOperationsExtensionsFloatTest',
	#superclass : 'TestCase',
	#category : 'Math-Operations-Extensions-Tests',
	#package : 'Math-Operations-Extensions-Tests'
}

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testArcTan [

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

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testDegreeCos [

	45.0 degreeCos. "Following tests use approximate equality, because cosine are generally evaluated using inexact Floating point arithmetic"
	self assert:
		(45.0 degreeCos squared - 0.5) abs <= Float machineEpsilon.
	self assert: (60.0 degreeCos - 0.5) abs <= Float machineEpsilon.
	self assert: (120.0 degreeCos + 0.5) abs <= Float machineEpsilon.
	-360.0 to: 360.0 do: [ :i | 
	self assert: (i degreeCos closeTo: i degreesToRadians cos) ]. "Following tests use strict equality which is a requested property of degreeCos"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeCos equals: 0.
		self assert: (k * 360 - 90) degreeCos equals: 0.
		self assert: (k * 360 + 180) degreeCos + 1 equals: 0.
		self assert: (k * 360) degreeCos - 1 equals: 0 ]
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testDegreeCosForExceptionalValues [

	self assert: Float nan degreeCos isNaN.
	self assert: Float infinity degreeCos isNaN.
	self assert: Float infinity negated degreeCos isNaN
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testDegreeSin [

	45.0 degreeSin. "Following tests use approximate equality, because sine are generally evaluated using inexact Floating point arithmetic"
	self assert:
		(45.0 degreeSin squared - 0.5) abs <= Float machineEpsilon.
	self assert: (30.0 degreeSin - 0.5) abs <= Float machineEpsilon.
	self assert: (-30.0 degreeSin + 0.5) abs <= Float machineEpsilon.
	-360.0 to: 360.0 do: [ :i | 
	self assert: (i degreeSin closeTo: i degreesToRadians sin) ]. "Following tests use strict equality which is a requested property of degreeSin"
	-10.0 to: 10.0 do: [ :k | 
		self assert: (k * 360 + 90) degreeSin - 1 equals: 0.
		self assert: (k * 360 - 90) degreeSin + 1 equals: 0.
		self assert: (k * 360 + 180) degreeSin equals: 0.
		self assert: (k * 360) degreeSin equals: 0 ]
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testDegreeSinForExceptionalValues [

	self assert: Float nan degreeSin isNaN.
	self assert: Float infinity degreeSin isNaN.
	self assert: Float infinity negated degreeSin isNaN
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testFloatPrinting [
	"This test shows that floats are printed exactly. The idea is too make sure that users understand that "

	self assert: (0.1+0.2) printString equals: '0.30000000000000004'.
	self assert: (-0.1-0.2) printString equals: '-0.30000000000000004'.
	self assert: 240 degreesToRadians cos equals: -0.5000000000000004.
  	self assert: 240 degreesToRadians cos abs equals: 0.5000000000000004
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testFloatRounded [
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

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testFloorLog2 [
	"Float internal representation of Float being in base 2, we expect (aFloat floorLog: 2) to be exact."

	| aBitLess aBitMore |
	aBitMore := 1 + Float machineEpsilon.
	aBitLess := 1 - Float machineEpsilon.
	Float emin + 1 to: Float emax - 1 do: [ :exp | 
		| exactPowerOfTwo |
		exactPowerOfTwo := 1.0 timesTwoPower: exp.
		self assert: (exactPowerOfTwo floorLog: 2) equals: exp.
		self assert: (exactPowerOfTwo * aBitMore floorLog: 2) equals: exp.
		self
			assert: (exactPowerOfTwo * aBitLess floorLog: 2)
			equals: exp - 1 ]
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testFractionAsFloat [
	"use a random test"

	| r m frac err collec |
	r := Random new seed: 1234567.
	m := (2 raisedTo: 54) - 1.
	200 timesRepeat: [
		frac := ((r nextInteger: m) * (r nextInteger: m) + 1) / ((r nextInteger: m) * (r nextInteger: m) + 1).
		err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
		self assert: err < (1/2)].

	collec := #(16r10000000000000 16r1FFFFFFFFFFFFF 1 2 16r20000000000000 16r20000000000001 16r3FFFFFFFFFFFFF 16r3FFFFFFFFFFFFE 16r3FFFFFFFFFFFFD).
	collec do: [:num |
		collec do: [:den |
			frac := Fraction numerator: num denominator: den.
			err := (frac - frac asFloat asTrueFraction) * frac reciprocal * (1 bitShift: 52).
			self assert: err <= (1/2)]]
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testHugeIntegerCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=7368"

	self deny: (1.0 closeTo: 200 factorial).
	self deny: (200 factorial closeTo: 1.0).
	self assert: (Float infinity closeTo: 200 factorial) equals: (200 factorial closeTo: Float infinity)
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testInfinityCloseTo [
	"This is a test for bug http://bugs.squeak.org/view.php?id=6729"

	self deny: (Float infinity closeTo: Float infinity negated).
	self deny: (Float infinity negated closeTo: Float infinity)
]

{ #category : 'testing' }
MathOperationsExtensionsFloatTest >> testRounding [

	self assert: (10.1234 round: 2) equals: 10.12.
 	self assert: (10.1234 round: 0) equals: 10
]
