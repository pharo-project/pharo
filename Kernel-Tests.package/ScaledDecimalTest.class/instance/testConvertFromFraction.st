tests
testConvertFromFraction

	| sd |
	sd := (13 / 11) asScaledDecimal: 6.
	self assert: ScaledDecimal == sd class.
	self assert: ('1.181818s6' = sd printString).
	self assert: 6 = sd scale.
	sd := (-13 / 11) asScaledDecimal: 6.
	self assert: ScaledDecimal == sd class.
	self assert: ('-1.181818s6' = sd printString).
	self assert: 6 = sd scale
