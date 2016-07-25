tests
testAsNumberWithSuperfluousDecimalPoint

	| sd |
	sd := '123.s2' asNumber.
	self deny: ScaledDecimal == sd class description: 'It used to, but this syntax is not valid Smalltalk'.

