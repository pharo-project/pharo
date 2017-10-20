tests - access
testSharedPools
	"self run: #testSharedPools"

	self assert: Point sharedPools = OrderedCollection new.
	self assert: Date sharedPools first = ChronologyConstants.
	self assert: Date sharedPools size = 1.	"a metaclass does not have shared pools since only classes have shared pools"
	Date class sharedPools.
	self assert: RootClassPoolUser sharedPools size = 1.
	self assert: ClassMultiplePoolUser sharedPools size = 2.	"has shared pools does not take into account the fact that a superclass may use some shared pools"
	self assert: SubclassPoolUser sharedPools isEmpty