testing
testAllReferencesToPool
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: FooSharedPool.
	self assert: result size = 1.