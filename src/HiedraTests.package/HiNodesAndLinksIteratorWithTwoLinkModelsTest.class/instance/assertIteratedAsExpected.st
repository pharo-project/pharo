running
assertIteratedAsExpected
	| builtArray |
	builtArray := self iteratedAsArray.
	
	self assert: builtArray equals: expectedArray