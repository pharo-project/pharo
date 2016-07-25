mathematical functions
raisedTo: aNumber 
	"Answer the receiver raised to aNumber."

	aNumber isInteger ifTrue: [
		"Do the special case of integer power"
		^ self raisedToInteger: aNumber].
	aNumber isFraction ifTrue: [
		"Special case for fraction power"
		^ (self nthRoot: aNumber denominator) raisedToInteger: aNumber numerator ].
	self < 0 ifTrue: [
		^ ArithmeticError signal: 'Negative numbers can''t be raised to float powers.' ].
	0 = aNumber ifTrue: [^ self class one].	"Special case of exponent=0"
	1 = aNumber ifTrue: [^ self].	"Special case of exponent=1"
	0 = self ifTrue: [				"Special case of self = 0"
		aNumber < 0
			ifTrue: [^ (ZeroDivide dividend: self) signal]
			ifFalse: [^ self]].
	^ (aNumber * self ln) exp		"Otherwise use logarithms"