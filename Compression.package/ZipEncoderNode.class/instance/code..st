accessing
code: aCode
	[aCode >= 0 and:[(1 bitShift: bitLength) > aCode]] assert.
	code := aCode.