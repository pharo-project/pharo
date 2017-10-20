tests
testSharedClosureEnvironment
	|blockArray|
	blockArray := self constructSharedClosureEnvironmentInDeadFrame.
	self assert: ((blockArray at: 2) value = 10).
	self assert: (((blockArray at: 1) value: 5) = 5).
	self assert: ((blockArray at: 2) value = 5).
