tests
testPrintShowingDecimalPlaces3
	"This problem were reported at http://bugs.squeak.org/view.php?id=7028
	unfortunate inversion of left / right padding"

	self assert: (1.009 printShowingDecimalPlaces: 3) = '1.009'.
	self assert: (35.900 printShowingDecimalPlaces: 3) = '35.900'.
	self assert: (-0.097 printShowingDecimalPlaces: 3) = '-0.097'.