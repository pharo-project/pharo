tests
testDivideTwoByZeroSignalsZeroDivide
	[self testedTutorial divideTwoByZero.
	self fail] 
	on: ZeroDivide 
	do: []