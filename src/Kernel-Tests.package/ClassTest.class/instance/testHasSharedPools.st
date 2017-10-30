tests - access
testHasSharedPools
	"self run: #testHasSharedPools"

	self deny: Point hasSharedPools.
	self assert: Date hasSharedPools.
	
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: Date class hasSharedPools.
	
	self assert: RootClassPoolUser hasSharedPools.
	
	"has shared pools does not take into account the fact that a superclass may use some shared pools"
	self deny: SubclassPoolUser hasSharedPools.