tests
testAllCallsOn
	| calls |
	calls := ClassQueryTest allCallsOn.
	self assert: calls size equals: 1.
	self assert: calls first compiledMethod equals: ClassQueryTest>>#testAllCallsOn