tests
testTwoConstsInSameMethod

	| result1 result2 |
	result1 := self sumOfTwoConsts: [ 1 ] and: [ 2 ].
	result2 := self sumOfTwoConsts: nil and: nil.
	self assert: result1 equals: 3.
	self assert: result2 equals: 3.