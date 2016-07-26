testing
testPoolDefinition
	| pool newClass |
	pool := RGClassDefinition createSharedPoolNamed: #TextConstants.
	self assert: pool isPool.
	self assert: pool users isEmpty.
	self assert: pool parent equals: Smalltalk globals.
	newClass := RGClassDefinition named: #OrderedCollection.
	pool addUser: newClass.
	self assert: pool users size equals: 1.
	self assert: (pool includesUser: newClass).
	self assert: (newClass sharedPoolNamed: #TextConstants) isPoolVariable.
	pool := RGClassDefinition named: #TextConstants.
	pool superclassName: #SharedPool.
	self assert: pool isPool.
	self assert: pool users isEmpty