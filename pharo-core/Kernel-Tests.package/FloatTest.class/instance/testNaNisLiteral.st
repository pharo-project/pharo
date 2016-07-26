tests - NaN behavior
testNaNisLiteral
	self deny: Float nan isLiteral description: 'there is no literal representation of NaN'