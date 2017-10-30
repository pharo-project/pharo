tests
testFromCode
	| t1  |
	t1 := TextEmphasis fromCode: 2r10101.
	self assert: t1 value equals: 2r10101.