test shadowing
testInstanceVariableShadowing

	interactive := true.
	self initializeErrorMessage.
	text := 'var1 |var1|'.

	self compileWithFailBlock: [
					self assert: (errorMessage = 'Name already defined ->').
					self assert: (errorLocation = 7).
					self assert: (errorSource contents = 'var1 |var1|').
					^nil].
	self fail.

