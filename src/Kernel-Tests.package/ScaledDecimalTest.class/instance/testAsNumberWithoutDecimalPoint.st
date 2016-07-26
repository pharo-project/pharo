tests
testAsNumberWithoutDecimalPoint

	| sd |
	sd := '123s0' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 0.
	self assert: '123s0' = sd printString.
