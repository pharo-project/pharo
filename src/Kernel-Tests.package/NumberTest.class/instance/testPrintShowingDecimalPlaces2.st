tests
testPrintShowingDecimalPlaces2
	"This tests problems related to Float>>rounded and Float>>roundTo::
	- Float>>#rounded is inexact
	- Float>>#roundTo: might overflow"

	"5000000000000001.0 asTrueFraction = 5000000000000001.
	5000000000000001 highBit = 53.
	This number is represented exactly asFloat, it should print exactly"

	self assert: (5000000000000001.0 printShowingDecimalPlaces: 0) = '5000000000000001'.	"50000000000001.25 asTrueFraction = (200000000000005/4).
	200000000000005 highBit = 48, 4 isPowerOfTwo,
	So this number is also represented exactly as Float, it should print exactly.
	Beware: (50000000000001.25 / 0.01) rounded exhibit the same problem as above."
	self assert: (50000000000001.25 printShowingDecimalPlaces: 2) = '50000000000001.25'.	"This number is close to maximum float value"
	1.0e306 printShowingDecimalPlaces: 3