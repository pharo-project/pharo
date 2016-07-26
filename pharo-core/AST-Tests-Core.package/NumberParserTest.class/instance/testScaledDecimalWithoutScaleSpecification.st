tests - ScaledDecimal
testScaledDecimalWithoutScaleSpecification
	self assert: (NumberParser parse: '0.050s') = (1/20).
	self assert: (NumberParser parse: '0.050s') scale= 3.