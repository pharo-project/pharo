testing
testAllReferencesToPool
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: FooSharedPool.
	self assert: result isEmpty. "FooSharedPool is not in this environment"