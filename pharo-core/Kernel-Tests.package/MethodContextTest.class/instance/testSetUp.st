tests
testSetUp
	"Note: In addition to verifying that the setUp worked the way it was expected to, testSetUp is used to illustrate the meaning of the simple access methods, methods that are not normally otherwise 'tested'"
	
	self deny: aMethodContext isDead.
	self assert: aMethodContext home = aMethodContext.
	self assert: aMethodContext receiver = aReceiver.
	self assert: (aMethodContext method isKindOf: CompiledMethod).
	self assert: aMethodContext method = aCompiledMethod.
	self assert: aMethodContext methodNode selector = #rightCenter.
	self assert: aMethodContext client printString = 'MethodContextTest>>#testSetUp'.
