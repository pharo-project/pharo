tests
testCustomInstance
	| memoryLogger |
	self assert: GLMLogger instance == GLMLogger nullInstance.
	memoryLogger := GLMMemoryLogger new.
	GLMLogger instance: memoryLogger.
	self assert: GLMLogger instance == memoryLogger.
	GLMLogger reset.
	self assert: GLMLogger instance == GLMLogger nullInstance