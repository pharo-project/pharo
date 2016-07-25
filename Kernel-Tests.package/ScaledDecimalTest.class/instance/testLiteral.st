tests
testLiteral

	| sd |
	sd := 1.40s2.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '1.40s2' = sd printString