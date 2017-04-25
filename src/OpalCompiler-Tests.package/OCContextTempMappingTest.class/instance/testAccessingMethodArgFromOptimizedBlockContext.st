tests
testAccessingMethodArgFromOptimizedBlockContext
	
	| actual |
	actual := self fetchArgFromOptimizedBlockInsideFullBlock: #argValue extraArg: #anotherArg.
	self assert: actual equals: #argValue