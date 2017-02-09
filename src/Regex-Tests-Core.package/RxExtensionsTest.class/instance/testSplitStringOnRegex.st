tests
testSplitStringOnRegex
	self assert: ('foobar' splitOn: '[aeiou]+' asRegex)
		equals: #('f' 'b' 'r') asOrderedCollection