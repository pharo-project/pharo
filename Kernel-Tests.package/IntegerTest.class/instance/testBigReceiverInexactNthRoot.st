tests - mathematical functions
testBigReceiverInexactNthRoot
	"
	IntegerTest new testBigReceiverInexactNthRoot
	"

	"Inexact 3rd root (not a whole cube number), so a Float must be answered.
	However, receiver is too big for Float arithmethic."
	| bigNum result |
	bigNum := (100 factorial raisedTo: 3) + 1.		"Add 1 so it is not a whole cube"
	self assert: bigNum asFloat isInfinite.			"Otherwise, we chose a bad sample"
	result := bigNum nthRoot: 3.
	self assert: result isFloat.
	self deny: result isInfinite.
	self assert: result = 100 factorial asFloat.		"No other float is closer. See following line"
	self assert: 100 factorial asFloat = (100 factorial+1) asFloat