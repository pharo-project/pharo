tests - ScaledDecimal
testScaledDecimalWithTrailingZeroes
	"This is a non regression tests for http://bugs.squeak.org/view.php?id=7169"
	
	self assert: (NumberParser parse: '0.50s2') = (1/2).
	self assert: (NumberParser parse: '0.500s3') = (1/2).
	self assert: (NumberParser parse: '0.050s3') = (1/20).