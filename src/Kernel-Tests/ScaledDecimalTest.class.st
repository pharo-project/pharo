"
I provide a test suite for ScaledDecimal values. Examine my tests to see how SmallIntegers should behave, and see how to use them.
"
Class {
	#name : #ScaledDecimalTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #tests }
ScaledDecimalTest >> testAsNumber [
	"Ensure no loss of precision"

	| sd |
	sd := '1.40s2' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '1.40s2' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberNegatedWithoutDecimalPoint [

	| sd |
	sd := '-123s0' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 0.
	self assert: '-123s0' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberNegatedWithoutDecimalPoint2 [

	| sd |
	sd := '-123s2' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '-123.00s2' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberWithExtendedScale [

	| sd |
	sd := '123s2' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '123.00s2' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberWithRadix [

	| sd |
	sd := '10r-22.2s5' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 5.
	self assert: '-22.20000s5' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberWithSuperfluousDecimalPoint [

	| sd |
	sd := '123.s2' asNumber.
	self deny: ScaledDecimal == sd class description: 'It used to, but this syntax is not valid Smalltalk'.


]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberWithoutDecimalPoint [

	| sd |
	sd := '123s0' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 0.
	self assert: '123s0' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testAsNumberWithoutDecimalPoint2 [

	| sd |
	sd := '123s2' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '123.00s2' = sd printString.

]

{ #category : #tests }
ScaledDecimalTest >> testCoercion [
	#( #* #+ #- #/) do: [:op |
		self assert: (1.0s1 perform: op with: 2) class = ScaledDecimal.
		self assert: (1.0s1 perform: op with: 1/2) class = ScaledDecimal.
		self deny: (1.0s1 perform: op with: 1.0) class = ScaledDecimal.
		
		self assert: (1 perform: op with: 2.0s1) class = ScaledDecimal.
		self assert: (1/2 perform: op with: 2.0s1) class = ScaledDecimal.
		self deny: (1.0 perform: op with: 1.0s1) class = ScaledDecimal]
]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromDecimalFraction [
	"Converting a Fraction with asScaledDecimal use strictly necessary number of decimal places when possible."
	0 to: 11 do: [:pow2 |
		0 to: 11 do: [:pow5 |
			| fraction sd sd2 |
			fraction := 13 / (2 raisedTo: pow2) / (5 raisedTo: pow5).
			sd := fraction asScaledDecimal.
			self assert: sd scale = (pow2 max: pow5).
			sd2 := ScaledDecimal readFrom: sd printString.
			self assert: sd = sd2]].
]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromFloat [

	| aFloat sd f2 diff |
	aFloat := 11/13 asFloat.
	sd := aFloat asScaledDecimal: 2.
	self assert: 2 = sd scale.
	self assert: '0.85s2' = sd printString.
	self assert: '-0.85s2' = sd negated printString.
	f2 := sd asFloat.
	diff := f2 - aFloat.
	self assert: diff abs < 1.0e-9. "actually, f = f2, but this is not a requirement"

]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromFraction [

	| sd |
	sd := (13 / 11) asScaledDecimal: 6.
	self assert: ScaledDecimal == sd class.
	self assert: ('1.181818s6' = sd printString).
	self assert: 6 = sd scale.
	sd := (-13 / 11) asScaledDecimal: 6.
	self assert: ScaledDecimal == sd class.
	self assert: ('-1.181818s6' = sd printString).
	self assert: 6 = sd scale

]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromInteger [
	"Converting an Integer with asScaledDecimal use strictly necessary number of decimal places: 0."

	| sd |
	sd := 13 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('13s0' = sd printString).
	sd := -13 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('-13s0' = sd printString).
	sd := 130000000013 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('130000000013s0' = sd printString).
	sd := -130000000013 asScaledDecimal.
	self assert: 0 = sd scale.
	self assert: ('-130000000013s0' = sd printString)

]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromIntegerWithScale [
	"Converting an Integer with asScaledDecimal: does now honour the scale passed as message argument."

	| sd |
	sd := 13 asScaledDecimal: 6.
	self assert: 6 = sd scale.
	self assert: ('13.000000s6' = sd printString).
	sd := -13 asScaledDecimal: 4.
	self assert: 4 = sd scale.
	self assert: ('-13.0000s4' = sd printString).
	sd := 130000000013 asScaledDecimal: 3.
	self assert: 3 = sd scale.
	self assert: ('130000000013.000s3' = sd printString).
	sd := -130000000013 asScaledDecimal: 1.
	self assert: 1 = sd scale.
	self assert: ('-130000000013.0s1' = sd printString)

]

{ #category : #tests }
ScaledDecimalTest >> testConvertFromNonDecimalFraction [
	"Converting a Fraction with asScaledDecimal use default number of decimals when the serie of decimals is infinite."
	| defaultNumberOfDecimals |
	defaultNumberOfDecimals := (1/3) asScaledDecimal scale.
	#(6 7 9 11 12 13 14 17 18 19 21 22 23 24) do: [:den |
		| sd sd2 |
		sd := (1/den) asScaledDecimal.
		self assert: sd scale = defaultNumberOfDecimals.
		sd2 := ScaledDecimal readFrom: sd printString.
		self deny: sd = sd2
		] 
]

{ #category : #tests }
ScaledDecimalTest >> testExactNthRoot [
	| eight thousandth tenth two |
	eight := 8.0s1.
	two := eight raisedTo: 1/3.
	self assert: two = 2.
	self assert: (two class = eight class and: [two scale = eight scale]).
	thousandth := 0.001s3.
	tenth := thousandth raisedTo: 1/3.
	self assert: tenth * 10 = 1.
	self assert: (tenth class = thousandth class and: [tenth scale = thousandth scale]).
]

{ #category : #tests }
ScaledDecimalTest >> testExactSqrt [
	| four hundredth tenth two |
	four := 4.0s1.
	two := four sqrt.
	self assert: two = 2.
	self assert: (two class = four class and: [two scale = four scale]).
	hundredth := 0.01s2.
	tenth := hundredth sqrt.
	self assert: tenth * 10 = 1.
	self assert: (tenth class = hundredth class and: [tenth scale = hundredth scale]).
]

{ #category : #tests }
ScaledDecimalTest >> testInexactNthRoot [
	| tenth cubicRoot3 fifthRootTenth three |
	three := 3.0s1.
	cubicRoot3 := three raisedTo: 1/3.
	self assert: cubicRoot3 isFloat.
	self deny: cubicRoot3 squared = 3.
	tenth := 0.10s2.
	fifthRootTenth := tenth raisedTo: 1/5.
	self assert: fifthRootTenth isFloat.
	self deny: fifthRootTenth squared = tenth.
]

{ #category : #tests }
ScaledDecimalTest >> testInexactSqrt [
	| tenth sqrt3 sqrtTenth three |
	three := 3.0s1.
	sqrt3 := three sqrt.
	self assert: sqrt3 isFloat.
	self deny: sqrt3 squared = 3.
	tenth := 0.10s2.
	sqrtTenth := tenth sqrt.
	self assert: sqrtTenth isFloat.
	self deny: sqrtTenth squared = tenth.
]

{ #category : #tests }
ScaledDecimalTest >> testIsLiteral [
	"This test is related to http://bugs.squeak.org/view.php?id=6796"
	
	self assert: 1.00s2 isLiteral description: 'every literal obviously isLiteral'.
	
	"Note that (1 / 3.00s2) is not a well behaved literal,
	because it does not re-evaluate to self...
	Every literal should be evaluated as self (see isSelfEvaluating).
	There is currently no way to print it as a literal.
	So i propose it shall not answer true."
	self deny: (1/3.00s2) isLiteral description: 'this number cannot represent itself as a literal'.
]

{ #category : #tests }
ScaledDecimalTest >> testLiteral [

	| sd |
	sd := 1.40s2.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '1.40s2' = sd printString
]

{ #category : #tests }
ScaledDecimalTest >> testOneRaisedToInteger [
	"One might be handled specially"
	
	self assert: (1.0s1 raisedToInteger: -1) scale = 1.
	self assert: (1.0s1 raisedToInteger: -1) = 1.
	self assert: (1.0s1 raisedToInteger: 0) scale = 1.
	self assert: (1.0s1 raisedToInteger: 0) = 1.
	self assert: (1.0s1 raisedToInteger: 1) scale = 1.
	self assert: (1.0s1 raisedToInteger: 1) = 1.
	self assert: (1.0s1 raisedToInteger: 2) scale = 1.
	self assert: (1.0s1 raisedToInteger: 2) = 1.
]

{ #category : #tests }
ScaledDecimalTest >> testPrintString [
	"The printed representation of a ScaledDecimal is rounded.
	Note that old implementation was truncated."

	| sd |
	sd := (13 / 11) asScaledDecimal: 6.
	self assert: ('1.181818s6' = sd printString).
	sd := (13 / 11) asScaledDecimal: 5.
	self assert: ('1.18182s5' = sd printString).
	sd := (13 / 11) asScaledDecimal: 5.
	self deny: ('1.18181s5' = sd printString)

]

{ #category : #tests }
ScaledDecimalTest >> testRaisedToInteger [
	"Raising to integer should preserve class and scale"
	
	self assert: (3.0s1 raisedToInteger: -1) scale = 1.
	self assert: (3.0s1 raisedToInteger: -1) = (1/3).
	self assert: (3.0s1 raisedToInteger: 0) scale = 1.
	self assert: (3.0s1 raisedToInteger: 0) = 1.
	self assert: (3.0s1 raisedToInteger: 1) scale = 1.
	self assert: (3.0s1 raisedToInteger: 1) = 3.
	self assert: (3.0s1 raisedToInteger: 2) scale = 1.
	self assert: (3.0s1 raisedToInteger: 2) = 9.
]

{ #category : #tests }
ScaledDecimalTest >> testReadFrom [
	"This is related to http://bugs.squeak.org/view.php?id=6779"
	
	self should: [(ScaledDecimal readFrom: '5.3') isKindOf: ScaledDecimal]
		description: 'Reading a ScaledDecimal should answer a ScaledDecimal'.
	self should: [((ScaledDecimal readFrom: '5.3') asScaledDecimal: 1) = (53/10 asScaledDecimal: 1)]
		description: 'ScaledDecimal readFrom: should not use Float intermediate because it would introduce round off errors'.
]

{ #category : #tests }
ScaledDecimalTest >> testRounded [
	self assert: (1999/1000s2) printString = '2.00s2'.
	self assert: (-1999/1000s2) printString = '-2.00s2'
]

{ #category : #tests }
ScaledDecimalTest >> testScaleExtension [
	"The scale is extended to the larger one in case of arithmetic operation"
	
	#( #* #+ #- #/) do: [:op |
		self assert: (2.5s1 perform: op with: 1.000s3) scale = 3.
		self assert: (3.5000s4 perform: op with: 1.0s1) scale = 4.]
]

{ #category : #tests }
ScaledDecimalTest >> testZeroRaisedToInteger [
	"Zero might be handle specially"
	
	self should: [0.0s1 raisedToInteger: -1] raise: Error.
	self assert: (0.0s1 raisedToInteger: 0) = 1.
	self assert: (0.0s1 raisedToInteger: 0) scale = 1.
	self assert: (0.0s1 raisedToInteger: 1) = 0.
	self assert: (0.0s1 raisedToInteger: 1) scale = 1.
	self assert: (0.0s1 raisedToInteger: 2) = 0.
	self assert: (0.0s1 raisedToInteger: 2) scale = 1.
]
