testing
testAllReferencesToPoolWhenNoRefences
	| result |
	
	result := self systemNavigationToTest allReferencesToPool: UnreferencedSharedPool.
	self assert: result size = 0.