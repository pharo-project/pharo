"
SUnit tests for numbers
"
Class {
	#name : #NumberTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #test }
NumberTest >> testAsInteger [
	"ensure we keep using truncated in asInteger"
	self assert: 0.5 asInteger equals: 0.
	self assert: (1/2) asInteger equals: 0
	
]

{ #category : #tests }
NumberTest >> testFractionPart [
	self
		assert: 2 fractionPart equals: 0;
		assert: (1 / 2) fractionPart equals: 1 / 2;
		assert: (4 / 3) fractionPart equals: 1 / 3;
		assert: 2.0 fractionPart equals: 0.0;
		assert: 0.5 fractionPart equals: 0.5;
		assert: 2.5 fractionPart equals: 0.5
]

{ #category : #tests }
NumberTest >> testIntegerPart [
	self
		assert: 2 integerPart equals: 2;
		assert: (1 / 2) integerPart equals: 0;
		assert: (4 / 3) integerPart equals: 1;
		assert: 2.0 integerPart equals: 2.0;
		assert: 0.5 integerPart equals: 0.0;
		assert: 2.5 integerPart equals: 2.0
]

{ #category : #tests }
NumberTest >> testNew [

	self should: [Number new] raise: self defaultTestError. 
]

{ #category : #tests }
NumberTest >> testOne [
	self
		assert: Integer one equals: 1;
		assert: Float one equals: 1.0;
		assert: Fraction one equals: 1
]

{ #category : #tests }
NumberTest >> testPrintShowingDecimalPlaces [
	self assert: (111.2 printShowingDecimalPlaces: 2) equals: '111.20'.
	self assert: (111.2 printShowingDecimalPlaces: 0) equals: '111'.
	self assert: (111 printShowingDecimalPlaces: 0) equals: '111'.
	self assert: (111111111111111 printShowingDecimalPlaces: 2) equals: '111111111111111.00'.
	self assert: (10 printShowingDecimalPlaces: 20) equals: '10.00000000000000000000'.
	self assert: (0.98 printShowingDecimalPlaces: 2) equals: '0.98'.
	self assert: (-0.98 printShowingDecimalPlaces: 2) equals: '-0.98'.
	self assert: (2.567 printShowingDecimalPlaces: 2) equals: '2.57'.
	self assert: (-2.567 printShowingDecimalPlaces: 2) equals: '-2.57'.
	self assert: (0.01 printShowingDecimalPlaces: 2) equals: '0.01'.
	self assert: (-0.001 printShowingDecimalPlaces: 2) equals: '0.00'
]

{ #category : #tests }
NumberTest >> testPrintShowingDecimalPlaces2 [
	"This tests problems related to Float>>rounded and Float>>roundTo::
	- Float>>#rounded is inexact
	- Float>>#roundTo: might overflow"

	"5000000000000001.0 asTrueFraction = 5000000000000001.
	5000000000000001 highBit = 53.
	This number is represented exactly asFloat, it should print exactly"

	self assert: (5000000000000001.0 printShowingDecimalPlaces: 0) equals: '5000000000000001'.	"50000000000001.25 asTrueFraction = (200000000000005/4).
	200000000000005 highBit = 48, 4 isPowerOfTwo,
	So this number is also represented exactly as Float, it should print exactly.
	Beware: (50000000000001.25 / 0.01) rounded exhibit the same problem as above."
	self assert: (50000000000001.25 printShowingDecimalPlaces: 2) equals: '50000000000001.25'.	"This number is close to maximum float value"
	1.0e306 printShowingDecimalPlaces: 3
]

{ #category : #tests }
NumberTest >> testPrintShowingDecimalPlaces3 [
	"This problem were reported at http://bugs.squeak.org/view.php?id=7028
	unfortunate inversion of left / right padding"

	self assert: (1.009 printShowingDecimalPlaces: 3) equals: '1.009'.
	self assert: (35.900 printShowingDecimalPlaces: 3) equals: '35.900'.
	self assert: (-0.097 printShowingDecimalPlaces: 3) equals: '-0.097'
]

{ #category : #tests }
NumberTest >> testRaisedTo [
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.
]

{ #category : #tests }
NumberTest >> testRaisedToInteger [
	self
		assert: (2 raisedToInteger: 0) equals: 1;
		assert: (2 raisedToInteger: 1) equals: 2;
		assert: (2 raisedToInteger: 4) equals: 16;
		assert: (0 raisedToInteger: 0) equals: 1;
		assert: (0 raisedToInteger: 2) equals: 0;
		assert: (2 raisedToInteger: -1) equals: 1 / 2;
		assert: (2 raisedToInteger: -4) equals: 1 / 16.

	self
		assert: (-3 raisedTo: 0) equals: 1;
		assert: (-3 raisedTo: 1) equals: -3;
		assert: (-3 raisedTo: 2) equals: 9;
		assert: (-3 raisedTo: 3) equals: -27;
		assert: (-3 raisedTo: -2) equals: 1 / 9;
		assert: (-3 raisedTo: -3) equals: -1 / 27.

	self should: [ 0 raisedTo: -1 ] raise: ZeroDivide
]

{ #category : #tests }
NumberTest >> testRaisedToIntegerWithFloats [
	self
		assert: (2.0 raisedToInteger: 0) equals: 1.0;
		assert: (2.0 raisedToInteger: 1) equals: 2.0;
		assert: (2.0 raisedToInteger: 4) equals: 16.0;
		assert: (0.0 raisedToInteger: 0) equals: 1.0;
		assert: (0.0 raisedToInteger: 2) equals: 0.0;
		assert: (2.0 raisedToInteger: -1) equals: 0.5;
		assert: (2.0 raisedToInteger: -4) equals: 0.0625.

	self
		assert: (-3.0 raisedTo: 0) equals: 1.0;
		assert: (-3.0 raisedTo: 1) equals: -3.0;
		assert: (-3.0 raisedTo: 2) equals: 9.0;
		assert: (-3.0 raisedTo: 3) equals: -27.0;
		assert: (-2.0 raisedTo: -2) equals: 0.25;
		assert: (-2.0 raisedTo: -3) equals: -0.125.

	self should: [ 0.0 raisedTo: -1 ] raise: ZeroDivide
]

{ #category : #tests }
NumberTest >> testReadFrom [
	self assert: 1.0e-14 equals: (Number readFrom: '1.0e-14').
	"Use a literal number format that compiles in GemStone.
	 Note that the test still checks the same value."
	self assert: 16r4000000 equals: (Number readFrom: '2r1e26').
	self should: [ Number readFrom: 'foo' ] raise: Error
]

{ #category : #tests }
NumberTest >> testReciprocal [
	self
		assert: 1 reciprocal equals: 1;
		assert: 2 reciprocal equals: 1 / 2;
		assert: -1 reciprocal equals: -1;
		assert: -3 reciprocal equals: -1 / 3.

	self should: [ 0 reciprocal ] raise: ZeroDivide
]

{ #category : #test }
NumberTest >> testRounded [

	self assert: 0 rounded equals: 0.
	self assert: 1 rounded equals: 1.
	self assert: (1/2) rounded equals: 1.
	self assert: ((1/2) - Float epsilon) rounded equals: 0.
]
