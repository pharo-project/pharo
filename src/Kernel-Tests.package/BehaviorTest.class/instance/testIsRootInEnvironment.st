tests
testIsRootInEnvironment
	self assert: ProtoObject isRootInEnvironment.
	self deny: Object isRootInEnvironment