tests
testCacheResetPreserveLinks
	self annotatedMethod.
	self assert: counter equals: 1.
	ASTCache reset.
	self assert: (self class >> #annotatedMethod) ast statements last value links anyOne == link.
	self annotatedMethod.
	self assert: counter equals: 2