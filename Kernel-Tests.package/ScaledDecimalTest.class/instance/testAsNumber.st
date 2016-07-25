tests
testAsNumber
	"Ensure no loss of precision"

	| sd |
	sd := '1.40s2' asNumber.
	self assert: ScaledDecimal == sd class.
	self assert: sd scale = 2.
	self assert: '1.40s2' = sd printString.
