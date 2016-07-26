testing
testAssertEqualsActualAndExpectedOrder
	self 
		should: [
			self assert: 'actual' equals: 'expected with fairly long print string'
		]
		raise: TestFailure 
		whoseDescriptionIncludes: 'Got ''actual'' instead of ''expected with fairly long print string''.' 
		description: 'In #assert:equals: the first argument is the actual value and the second one is the expected.'.