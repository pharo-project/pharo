tests - mathematical functions
testBigReceiverInexactSqrt
	"
	IntegerTest new testBigReceiverInexactSqrt
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum := 100 factorial squared + 1.		"Add 1 so it is not a whole square"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result := bigNum sqrt.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following lines"
	self assert: (result successor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.
	self assert: (result predecessor asFraction squared - bigNum) abs >= (result asFraction squared - bigNum) abs.