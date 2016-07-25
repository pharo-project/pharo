tests
testPush
	self assert: (self stackSize = 0).
	self deny: packageA isInStack.
	tarjanPackage push: packageA.
	self assert: (self stackSize = 1).
	self assert: packageA isInStack.
	