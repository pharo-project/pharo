tests - ScaledDecimal
testScaledDecimalWithoutScaleSpecification
	self assert: (Number readFrom: '0.050s') = (1/20).
	self assert: (Number readFrom: '0.050s') scale= 3.
	
	self assert: (ScaledDecimal readFrom: '0.050') = (1/20).
	self assert: (ScaledDecimal readFrom: '0.050') scale= 3.